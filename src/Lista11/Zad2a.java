// Kamil Herbetko
package Lista11;


public class Zad2a {
    private static IntCell n = new IntCell();

    static class IntCell {
        private int n = 0;
        private boolean ifAvaliable = true;

        synchronized public int getN() {
            while (!ifAvaliable){
                try {
                    wait();
                } catch (InterruptedException e){
                    e.printStackTrace();
                }
            }
            ifAvaliable = false;
            return n;
        }

        synchronized public void setN(int n) {
            ifAvaliable = true;
            this.n = n;
            notify();
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
