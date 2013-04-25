package language.shootout;
import static org.junit.Assert.*;

import java.util.concurrent.ForkJoinPool;

import org.junit.Test;

public class CountSummaryTest {
    
    private final ForkJoinPool forkJoinPool = new ForkJoinPool();
    
    @Test
    public void testBalanced() {
        
        int size = 1000;
        do {
            String data = blancedString(size);
            CountSummary summary = new CountSummary(data, forkJoinPool);
            
            int quarter   = size/4;
            int remainder = size%4;
            
            assertEquals(quarter + (remainder-- >0 ? 1 : 0),summary.getCountT());
            assertEquals(quarter + (remainder-- >0 ? 1 : 0),summary.getCountG());
            assertEquals(quarter + (remainder-- >0 ? 1 : 0),summary.getCountC());
            assertEquals(quarter + (remainder-- >0 ? 1 : 0),summary.getCountA());
        
        } while(--size>=0);
    }
    
    @Test
    public void testFrontHeavy() {
        
        int size = 1000;
        do {
            String data = frontHeavyString(size);
            CountSummary summary = new CountSummary(data, forkJoinPool);
            
            int half      = size/2;
            int quarter   = size/4;
            int eighth    = size/8;

            assertEquals(eighth +  ((size&0x7)==0 ? 0 : 1), summary.getCountT());
            assertEquals(eighth  + ((size&0x7)>4  ? 1 : 0), summary.getCountG());
            assertEquals(quarter + ((size&0x3)==3 ? 1 : 0), summary.getCountC());
            assertEquals(half,                              summary.getCountA());
        
        } while(--size>=0);
    }
    
    @Test
    public void testAllSame() {
        
        int size = 1000;
        
        char[] testChars = new char[] {'A','C','G','T'};
        
        
        do {
            int[][] expected = new int[][] {{0,0,0,size},
                                            {0,0,size,0},
                                            {0,size,0,0},
                                            {size,0,0,0}};
            int j = 4;
            while (--j>=0) {
                    
                String data = solidString(size,testChars[j]);
                CountSummary summary = new CountSummary(data, forkJoinPool);
                
                assertEquals(expected[j][0],     summary.getCountT());
                assertEquals(expected[j][1],     summary.getCountG());
                assertEquals(expected[j][2],     summary.getCountC());
                assertEquals(expected[j][3],     summary.getCountA());
            }
        } while(--size>=0);
    }
    
    private String solidString(int size, char c) {
        StringBuffer testString = new StringBuffer(size);
        int i = size;
        while (--i>=0) {
            testString.append(c);
        }
        return testString.toString();
    }
    
    private String frontHeavyString(int size) {
        return testString(size, 0x1, 0x2, 0x4);
    }
    
    private String blancedString(int size) {
        return testString(size, 0x3, 0x2, 0x1);
    }
    
    private String testString(int size, int mask1, int mask2, int mask3) {

        StringBuilder testString = new StringBuilder(size);
        int i = size;
        while (--i>=0) {
            if ((i&mask1) == mask1) {
                
                testString.append('A');
                
            } else if ((i&mask2) == mask2) {
                
                testString.append('C');
                
            } else if ((i&mask3) == mask3) {
                
                testString.append('G');
                
            } else {
                
                testString.append('T');
                
            }
        }
        return testString.toString();
    }
    
}
