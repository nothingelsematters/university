package ru.ifmo.rain.naumov.walk;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Path;

public class FNVHasher {
    public static int getHash(Path fileName) {
        final int PRIME = 0x01000193;
        int hval = 0x811c9dc5;

        try (FileInputStream fileInputStream = new FileInputStream(fileName.toString())) {
            byte[] buffer = new byte[1024];

            int readCount;
            while ((readCount = fileInputStream.read(buffer)) >= 0) {
                for (int i = 0; i < readCount; ++i) {
                    hval *= PRIME;
                    hval ^= (buffer[i] & 0xFF);
                }
            }

            return hval;

        } catch (IOException e) {
            return 0;
        }
    }
}
