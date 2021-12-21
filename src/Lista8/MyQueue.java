package Lista8;// Kamil Herbetko
// Zadanie 1
import Lista8.Exceptions.EmptyException;
import Lista8.Exceptions.FullException;

import java.util.ArrayList;

public class MyQueue<E> implements Lista8.Interface.MyQueue<E> {
    static private final int default_size = 10;
    private final int size;
    private int index_of_first;
    private int index_of_last;
    private final ArrayList<E> queue;

    public MyQueue(int size) {
        this.size = size + 1;
        this.index_of_first = 0;
        this.index_of_last = 0;
        this.queue = new ArrayList<>(size + 1);
    }

    public MyQueue() {
        this.size = default_size;
        this.index_of_first = 0;
        this.index_of_last = 0;
        this.queue = new ArrayList<>(default_size);
    }

    @Override
    public void enqueue(E x) throws FullException {
        if ((index_of_last + 1) % size == index_of_first){
            throw new FullException("Class Lista8.MyQueue: operation enqueue applied to a full queue.");
        }
        else{
            if (queue.size() > index_of_last){
                queue.set(index_of_last, x);
            }
            else {
                queue.add(x);
            }
            index_of_last = (index_of_last + 1) % size;
        }
    }

    @Override
    public void dequeue() {
        if (index_of_last != index_of_first){
            index_of_first = (index_of_first + 1) % size;
        }
    }

    @Override
    public E first() throws EmptyException {
        if (index_of_first == index_of_last){
            throw new EmptyException("Class Lista8.MyQueue: operation first applied to an empty queue");
        }
        else{
            return queue.get(index_of_first);
        }
    }

    @Override
    public boolean isEmpty() {
        return index_of_first == index_of_last;
    }

    @Override
    public boolean isFull() {
        return (index_of_last + 1) % size == index_of_first;
    }
}
