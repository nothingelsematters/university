package ru.ifmo.rain.naumov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;
import static ru.ifmo.rain.naumov.hello.Utils.*;

import java.lang.NumberFormatException;
import java.util.stream.IntStream;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ArrayBlockingQueue;
import java.lang.InterruptedException;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.DatagramPacket;
import java.io.IOException;
import java.nio.charset.StandardCharsets;


public class HelloUDPServer implements HelloServer {
    private final static int POOL_LIMIT = 1000;
    private final static int KEEP_ALIVE = 10;
    private final static int AWAITING_TIME = 1;

    private DatagramSocket datagramSocket = null;
    private ExecutorService workers = null;
    private ExecutorService dispatcher = null;

    /**
     * {@inheritDoc}
     */
    @Override
    public void start(int port, int threads) {
        try {
            datagramSocket = new DatagramSocket(port);
        } catch (SocketException e) {
            System.err.println("Datagram Socket exception appeared: " + e.getMessage());
            return;
        }

        workers = new ThreadPoolExecutor(threads, threads, KEEP_ALIVE, TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(POOL_LIMIT), new ThreadPoolExecutor.DiscardOldestPolicy());

        dispatcher = Executors.newSingleThreadExecutor();
        dispatcher.submit(() -> {
            while (!datagramSocket.isClosed() && !Thread.currentThread().isInterrupted()) {
                try {
                    final DatagramPacket request = receiveDP(datagramSocket);
                    datagramSocket.receive(request);
                    workers.submit(() -> processRequest(request));
                } catch(IOException ignored) {
                    // ...
                }
            }
        });
    }

    private void processRequest(final DatagramPacket request) {
        final DatagramPacket respond = sendDP(request.getSocketAddress());
        respond.setData(sendingString("Hello, " + receivedString(request)));

        try {
            datagramSocket.send(respond);
        } catch(IOException e) {
            System.err.println("Responding exception occurred: " + e.getMessage());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        datagramSocket.close();
        workers.shutdownNow();
        dispatcher.shutdownNow();

        try {
            workers.awaitTermination(AWAITING_TIME, TimeUnit.SECONDS);
        } catch(InterruptedException ignored) {
            // ...
        }
    }

    private static void printUsage() {
        System.out.println("Expected non-null arguments: [port] [threads]");
    }


    /**
     * Runs a server with the following parameters:
     * [port] [threads]
     */
    public static void main(String[] args) {
        if (invalidArgs(args, 2)) {
            printUsage();
            return;
        }

        try (final HelloUDPServer server = new HelloUDPServer()) {
            server.start(integerArg(args, 0), integerArg(args, 1));
        } catch(IllegalArgumentException e) {
            printUsage();
        }
    }
}
