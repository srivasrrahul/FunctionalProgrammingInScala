class Foo {

    final Lock lock = new ReentrantLock();
    final Condition secondCondition  = lock.newCondition();
    final Condition thirdCondition = lock.newCondition();
    int count = 0;

    public Foo() {

    }

    public void first(Runnable printFirst) throws InterruptedException {
        lock.lock();
        try {
            // printFirst.run() outputs "first". Do not change or remove this line.
            printFirst.run();
            count = 1;
            secondCondition.signal();
        }finally {
            lock.unlock();
        }
    }

    public void second(Runnable printSecond) throws InterruptedException {

        lock.lock();
        try {
            while (count < 1) {
                secondCondition.await();
            }
            // printSecond.run() outputs "second". Do not change or remove this line.
            printSecond.run();
            count = 2;
            thirdCondition.signal();
        }finally {
            lock.unlock();
        }
    }

    public void third(Runnable printThird) throws InterruptedException {
        lock.lock();
        try {
            while (count < 2) {
                thirdCondition.await();
            }
            // printThird.run() outputs "third". Do not change or remove this line.
            printThird.run();
        }finally {
            lock.unlock();
        }
    }
}