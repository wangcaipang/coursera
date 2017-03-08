
import java.util.Iterator;

import edu.princeton.cs.algs4.StdOut;

public class Deque<Item> implements Iterable<Item> {
    private Item[] cap = (Item[]) new Object[1];
    private int index = 0;
    private int size = 0;

    public Deque() {

    }                          // construct an empty deque

    // is the deque empty?
    public boolean isEmpty() {
        return size == 0;
    }

    // return the number of items on the deque
    public int size() {
        return size;
    }

    // add the item to the front
    public void addFirst(Item item) {
        if (item == null) throw new java.lang.NullPointerException("");
        if (size == cap.length) {
            resize(size * 2);
        }
        cap[index] = item;
    }


    public void addLast(Item item)           // add the item to the end
    {
        if (item == null) throw new java.lang.NullPointerException("");
        if (first == last) first--;
        if (last == cap.length) {
            resizeAfter(cap.length + size());
        }
        cap[last++] = item;
    }

    public Item removeFirst() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        Item item = cap[++first];
        if (last == size() * 4) {
            resizeBefore(cap.length - size());
        } else if (cap.length - last == size() * 3) {
            resizeAfter(cap.length - size());
        }
        if (first == last - 1) last = first = 0;
        return item;
    }              // remove and return the item from the front

    public Item removeLast() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        Item item = cap[--last];
        if (last == size() * 4) {
            resizeBefore(cap.length - size());
        } else if (cap.length - last == size() * 3) {
            resizeAfter(cap.length - size());
        }
        if (first == last - 1) last = first = 0;
        return item;
    }             // remove and return the item from the end

    public Iterator<Item> iterator() {
        return new ReverseArrayIterator();
    }      // return an iterator over items in order from front to end

    private class ReverseArrayIterator implements Iterator<Item> {
        private int _first = first;
        private int _last = last;

        public boolean hasNext() {
            return _last > _first + 1;
        }

        public Item next() {
            if (_first + 1 == _last) throw new java.util.NoSuchElementException("");
            return cap[++_first];
        }
    }

    public static void main(String[] args) {
        Deque<Integer> deque = new Deque<Integer>();
        deque.size();
        deque.size();
        deque.addLast(2);
        deque.addFirst(3);
        deque.addFirst(4);
        deque.addLast(5);

        deque.addFirst(6);

        deque.addFirst(7);

        deque.addFirst(8);

        deque.addLast(9);

        deque.addFirst(10);

        deque.size();

    }
}

