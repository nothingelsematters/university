package com.github.nothingelsematters.sd.refactoring.config;

import org.cfg4j.provider.ConfigurationProviderBuilder;
import org.cfg4j.source.ConfigurationSource;
import org.cfg4j.source.classpath.ClasspathConfigurationSource;
import org.cfg4j.source.context.filesprovider.ConfigFilesProvider;

import java.nio.file.Paths;
import java.util.Collections;

public class ConfigurationImpl {

    private static final Configuration INSTANCE;

    static {
        ConfigFilesProvider configFilesProvider = () -> Collections.singletonList(Paths.get("application.properties"));
        ConfigurationSource source = new ClasspathConfigurationSource(configFilesProvider);

        INSTANCE =
            new ConfigurationProviderBuilder().withConfigurationSource(source).build().bind("", Configuration.class);
    }

    public static Configuration getInstance() {
        return INSTANCE;
    }
}
