// Kamil Herbetko
// Zadanie 1
package Lista8.Interface;

import Lista8.Exceptions.EmptyException;
import Lista8.Exceptions.FullException;

public interface MyQueue<E> {
    void enqueue(E x) throws FullException;
    void dequeue();
    E first() throws EmptyException;
    boolean isEmpty();
    boolean isFull();
}
