package queue;

public class LinkedQueue extends AbstractQueue implements Copiable {
private Node head;
private Node tail;

protected void enqueueImpl(Object element) {
        if (isEmpty()) {
          Node insert = new Node(element);
          head = insert;
          tail = head;
          return;
        }
        Node insert = new Node(element);
        tail.next = insert;
        tail = tail.next;
}

protected void remove() {
        head = head.next;
}

protected Object elementImpl() {
        return head.value;
}

public LinkedQueue makeCopy() {
        final LinkedQueue copy = new LinkedQueue();
        copy.size = size;
        copy.head = head;
        copy.tail = tail;
        return copy;
}

public void clearImpl() {
        head = null;
        tail = head;
}

private class Node {
      private Object value;
      private Node next;

      public Node(Object value) {
          assert value != null;
          this.value = value;
        }
}
}
