package ru.ifmo.rain.naumov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;
import static ru.ifmo.rain.naumov.hello.Utils.*;

import java.lang.NumberFormatException;
import java.util.stream.IntStream;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.net.SocketAddress;
import java.net.InetSocketAddress;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.DatagramPacket;
import java.nio.charset.StandardCharsets;
import java.io.IOException;

public class HelloUDPClient implements HelloClient {
    private static final int TIMEOUT = 500;

    /**
     *  {@inheritDoc}
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        final InetAddress address;
        try {
            address = InetAddress.getByName(host);
        } catch(UnknownHostException e) {
            System.err.println("Can't resolve host's name");
            return;
        }

        final ExecutorService workers = Executors.newFixedThreadPool(threads);
        final SocketAddress socketAddress = new InetSocketAddress(address, port);

        IntStream.range(0, threads).forEach(index -> workers.submit(() -> performRequest(socketAddress, prefix, index, requests)));
        workers.shutdown();
        try {
            System.out.println(workers.awaitTermination(threads * requests, TimeUnit.SECONDS) ? "Done successfully" : "Timeout exceeded");
        } catch (InterruptedException ignored) {
            System.out.println("Interrupted");
        }
    }

    private static void performRequest(final SocketAddress socketAddress, final String prefix, final int index, final int requests) {
        try (final DatagramSocket datagramSocket = new DatagramSocket()) {
            datagramSocket.setSoTimeout(TIMEOUT);
            final DatagramPacket request = sendDP(socketAddress);
            final DatagramPacket respond = receiveDP(datagramSocket);

            for (int i = 0; i < requests; ++i) {
                final String requestText = String.format("%s%d_%d", prefix, index, i);
                request.setData(sendingString(requestText));

                while (!datagramSocket.isClosed() && !Thread.currentThread().isInterrupted()) {
                    try {
                        datagramSocket.send(request);
                        System.out.println("Request: " + requestText);
                        datagramSocket.receive(respond);
                    } catch(IOException e) {
                        System.err.println("Failed to perform a request / get a respond: " + e.getMessage());
                        continue;
                    }

                    final String respondText = receivedString(respond);
                    System.out.println("Respond: " + respondText);
                    if ((respondText.length() > requestText.length()) && respondText.contains(requestText)) {
                        break;
                    }
                }
            }
        } catch (SocketException e) {
            System.err.println("Datagram Socket exception appeared: " + e.getMessage());
        }
    }

    private static void printUsage() {
        System.out.println("5 non-null arguments expected\n" +
            "Usage: [Name/ip-address] [port] [prefix] [threads amount] [query amount]");
    }

    /**
     *  Runs the client with the following parameters:
     *  [Name/ip-address] [port] [prefix] [threads amount] [query amount]
     */
    public static void main(String[] args) {
        if (invalidArgs(args, 5)) {
            printUsage();
            return;
        }

        try {
            new HelloUDPClient().run(args[0], integerArg(args, 1), args[2], integerArg(args, 3), integerArg(args, 4));
        } catch (IllegalArgumentException e) {
            printUsage();
        }
    }
}
