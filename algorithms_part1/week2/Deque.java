
import java.util.Iterator;

import edu.princeton.cs.algs4.StdOut;

public class Deque<Item> implements Iterable<Item> {
    private Item[] headQueue = (Item[]) new Object[1];
    private Item[] lastQueue = (Item[]) new Object[1];
    private int hSize = 0;
    private int lSize = 0;
    private int hIndex = 0;
    private int lIndex = 0;

    public Deque() {

    }                          // construct an empty deque

    // is the deque empty?
    public boolean isEmpty() {
        return (hSize + lSize) == 0;
    }

    // return the number of items on the deque
    public int size() {
        return hSize + lSize;
    }

    // add the item to the front
    public void addFirst(Item item) {
        if (item == null) throw new java.lang.NullPointerException("");
        if (hSize + hIndex == headQueue.length) {
            resize(true, hSize == 0 ? 1 : hSize * 2);
        }
        headQueue[hSize + hIndex] = item;
        hSize++;
    }


    public void addLast(Item item)           // add the item to the end
    {
        if (item == null) throw new java.lang.NullPointerException("");
        if (lSize + lIndex == lastQueue.length) {
            resize(false, lSize == 0 ? 1 : lSize * 2);
        }
        lastQueue[lSize + lIndex] = item;
        lSize++;
    }

    public Item removeFirst() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        if (hSize == 0) {
            return dequeue(false);
        }
        if (hSize > 0 && hSize == headQueue.length / 4) {
            resize(true, 2 * hSize);
        }
        hSize--;
        Item item = headQueue[hSize + hIndex];
        return item;
    }              // remove and return the item from the front

    public Item removeLast() {
        if (isEmpty()) throw new java.util.NoSuchElementException("");
        if (lSize == 0) {
            return dequeue(true);
        }
        if (lSize > 0 && lSize == lastQueue.length / 4) {
            resize(false, lSize * 2);
        }
        lSize--;
        Item item = lastQueue[lSize + lIndex];
        return item;
    }             // remove and return the item from the end

    private void resize(boolean isHead, int newSize) {
        Item[] queue = headQueue;
        int index = hIndex;
        int size = hSize;
        if (!isHead) {
            queue = lastQueue;
            index = lIndex;
            size = lSize;
        }
        Item[] newQueue = (Item[]) new Object[newSize];
        for (int i = 0; i < size; i++) {
            newQueue[i] = queue[index + i];
        }
        if (isHead) {
            headQueue = newQueue;
            hIndex = 0;
        } else {
            lastQueue = newQueue;
            lIndex = 0;
        }
    }

    private Item dequeue(boolean isHead) {
        Item[] queue = headQueue;
        int index = hIndex;
        int size = hSize;
        if (!isHead) {
            queue = lastQueue;
            index = lIndex;
            size = lSize;
        }
        Item item = queue[index++];
        size--;
        if (isHead) {
            hSize = size;
            hIndex = index;
        } else {
            lSize = size;
            lIndex = index;
        }
        return item;
    }

    public Iterator<Item> iterator() {
        return new ReverseArrayIterator();
    }      // return an iterator over items in order from front to end

    private class ReverseArrayIterator implements Iterator<Item> {
        private int first = hIndex + hSize - 1;
        private int last = lIndex + lSize - 1;
        private int headIndex = first;
        private int lastIndex = lIndex;

        public boolean hasNext() {
            return headIndex >= hIndex || lastIndex <= last;
        }

        public Item next() {
            if (!hasNext()) throw new java.util.NoSuchElementException("");
            if (headIndex >= hIndex) {
                Item item = headQueue[headIndex--];
                return item;
            } else {
                Item item = lastQueue[lastIndex++];
                return item;
            }
        }
    }

    public static void main(String[] args) {
        Deque<Integer> deque = new Deque<Integer>();
        deque.addFirst(0);
        deque.removeLast();
        deque.addFirst(2);
        deque.size();
        for (int i : deque) {
            StdOut.printf("%d ", i);
        }
    }
}

