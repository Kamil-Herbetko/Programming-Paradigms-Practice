package Lista8;
// Kamil Herbetko
// Zadanie 1
import Lista8.Exceptions.EmptyException;
import Lista8.Exceptions.FullException;

public class Main {
    public static void main(String[] args) throws EmptyException, FullException {
        MyQueue<Integer> myQueue = new MyQueue<>(3);
        System.out.println(myQueue.isEmpty());
        myQueue.enqueue(5);
        myQueue.enqueue(6);
        myQueue.enqueue(7);
        System.out.println(myQueue.first());
        System.out.println(myQueue.isFull());
        try {
            myQueue.enqueue(1);
        }
        catch (FullException e){
            System.out.println(e.getMessage());
        }
        myQueue.dequeue();
        System.out.println(myQueue.first());
        myQueue.dequeue();
        System.out.println(myQueue.first());
        myQueue.dequeue();
        try {
            System.out.println(myQueue.first());
        }
        catch (EmptyException e){
            System.out.println(e.getMessage());
        }

        myQueue.enqueue(1);
        myQueue.enqueue(2);
        System.out.println(myQueue.first());
        myQueue.dequeue();
        System.out.println(myQueue.first());
    }
}
