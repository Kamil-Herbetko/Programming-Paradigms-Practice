// Kamil Herbetko
package Lista11;

import java.util.concurrent.Semaphore;

public class Zad2b {
    private static IntCell n = new IntCell();
    private static Semaphore sem = new Semaphore(1);

    static class IntCell {
        private int n = 0;


        public int getN() {
            try {
                sem.acquire();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            return n;
        }

        public void setN(int n) {
            this.n = n;
            sem.release();
        }
    }

    static class Count extends Thread {

        @Override
        public void run() {
            int temp;
            for (int i = 0; i < 200000; i++) {
                temp = n.getN();
                n.setN(temp + 1);
            }
        }
    }

    public static void main(String[] args) {
        Count p = new Count();
        Count q = new Count();
        p.start();
        q.start();
        try { p.join(); q.join(); }
        catch (InterruptedException e) { }
        System.out.println("The value of n is " + n.getN());
    }
}
