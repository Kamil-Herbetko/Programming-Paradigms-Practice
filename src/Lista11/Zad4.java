// Kamil Herbetko
package Lista11;

import java.util.Random;
import java.util.concurrent.Semaphore;

public class Zad4 {
    private static Semaphore[] sticks = {new Semaphore(1), new Semaphore(1), new Semaphore(1), new Semaphore(1), new Semaphore(1)};
    private static Semaphore guardian = new Semaphore(4);


    public static void eating(int index){
        try {
            guardian.acquire();
            sticks[index].acquire();
            sticks[(index + 1) % 5].acquire();

        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.printf("Now %s is eating. \n", Thread.currentThread().getName());

        sticks[index].release();
        sticks[(index + 1) % 5].release();
        guardian.release();
    }



    static class Philosopher extends Thread{
        private int index;
        private Random random;

        public Philosopher(String name, int index) {
            super(name);
            this.index = index;
            this.random = new Random();
        }

        @Override
        public void run() {
            for (int i = 0; i < 20000; i++) {
                System.out.printf("Now %s is meditating. \n", this.getName());
                try {
                    Thread.sleep(random.nextInt(5000));
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                eating(index);
            }
        }
    }

    public static void main(String[] args) {
        Philosopher philosopher0 = new Philosopher("Philosopher0", 0);
        Philosopher philosopher1 = new Philosopher("Philosopher1", 1);
        Philosopher philosopher2 = new Philosopher("Philosopher2", 2);
        Philosopher philosopher3 = new Philosopher("Philosopher3", 3);
        Philosopher philosopher4 = new Philosopher("Philosopher4", 4);


        philosopher0.start();
        philosopher1.start();
        philosopher2.start();
        philosopher3.start();
        philosopher4.start();
    }
}
