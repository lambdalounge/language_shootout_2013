package language.shootout;
import java.util.concurrent.RecursiveTask;


public class CountingTask extends RecursiveTask<Integer>{

    private final byte[] data;
    private final int start;
    private final int end;
    
    public CountingTask(byte[] data, int start, int end) {
        this.data = data;
        this.start = start;
        this.end = end;
    }
    
    @Override
    protected Integer compute() {
        int len = end-start;
        if (0==len) {
            return 0;
        }
        if (1==len) {
            return extractNucleotides(data[start]);
        }
        
        int middle = start + (len>>>1);
        CountingTask count1 = new CountingTask(data, start, middle);
        count1.fork();
        CountingTask count2 = new CountingTask(data, middle, end);
        return count2.compute()+count1.join();
    }

    private final int extractNucleotides(byte b) {
        //        x xx
        //A => 01000001
        //C => 01000011
        //G => 01000111
        //T => 01010100
        
        //             T             GT           CG
        return ((b&0x10)<<16)+((b&0x04)<<8)+((b&0x02)>>>1);
    }
}
