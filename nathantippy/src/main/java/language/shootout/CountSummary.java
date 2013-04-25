package language.shootout;

import java.util.concurrent.ForkJoinPool;

public class CountSummary {

    private final int countA;
    private final int countC;
    private final int countG;
    private final int countT;

    public static void main(String arg[]) {
        ForkJoinPool forkJoinPool = new ForkJoinPool();
        String dna = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC";
        CountSummary c = new CountSummary(dna,forkJoinPool);
        System.err.println(c.toString());
    }

    public CountSummary(String dna, ForkJoinPool forkJoinPool) {
        byte[] data = dna.getBytes();
        int total = forkJoinPool.invoke(new CountingTask(data,0,data.length) );
        
        int gt = (total>>>10)&0x3FF;
        int cg = total&0x3FF;
        
        countT = total>>>20;
        countG = gt - countT;
        countC = cg - countG;
        countA = data.length - cg - countT;

    }

    public String toString() {
        return " A:"+countA+" C:"+countC+" G:"+countG+" T:"+countT;
    }
    
    public int getCountA() {
        return countA;
    }
    
    public int getCountC() {
        return countC;
    }
    
    public int getCountG() {
        return countG;
    }
    
    public int getCountT() {
        return countT;
    }
}
