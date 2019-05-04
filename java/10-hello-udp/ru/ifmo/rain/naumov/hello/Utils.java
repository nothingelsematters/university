package ru.ifmo.rain.naumov.hello;


import java.lang.NumberFormatException;
import java.net.SocketAddress;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

class Utils {
    private static final Charset charset = StandardCharsets.UTF_8;

    public static boolean invalidArgs(String[] args, int size) {
        if (args == null || args.length != size) {
            return true;
        }
        for (String arg : args) {
            if (arg == null) {
                return true;
            }
        }
        return false;
    }

    public static int integerArg(String[] args, int index) throws NumberFormatException {
        return Integer.parseInt(args[index]);
    }

    public static DatagramPacket sendDP(final SocketAddress socketAddress) {
        return new DatagramPacket(new byte[0], 0, socketAddress);
    }

    public static DatagramPacket receiveDP(final DatagramSocket datagramSocket) throws SocketException {
        final int receiveBufferSize = datagramSocket.getReceiveBufferSize();
        return new DatagramPacket(new byte[receiveBufferSize], receiveBufferSize);
    }

    public static String receivedString(final DatagramPacket datagramPacket) {
        return new String(datagramPacket.getData(), datagramPacket.getOffset(), datagramPacket.getLength(), charset);
    }

    public static byte[] sendingString(final String string) {
        return string.getBytes(charset);
    }
}
