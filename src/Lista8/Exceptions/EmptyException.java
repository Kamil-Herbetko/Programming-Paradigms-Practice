// Kamil Herbetko
// Zadanie 1
package Lista8.Exceptions;

public class EmptyException extends Exception{
    public EmptyException(){
        super();
    }

    public EmptyException(String message) {
        super(message);
    }
}
