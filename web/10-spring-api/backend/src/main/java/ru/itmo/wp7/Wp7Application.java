package ru.itmo.wp7;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import ru.itmo.wp7.interceptor.AuthorizationInterceptor;

@SpringBootApplication
public class Wp7Application implements WebMvcConfigurer {
	private AuthorizationInterceptor authorizationInterceptor;

	@Autowired
	public void setApplicationInterceptor(AuthorizationInterceptor authorizationInterceptor) {
		this.authorizationInterceptor = authorizationInterceptor;
	}

	@Override
	public void addInterceptors(InterceptorRegistry registry) {
		registry.addInterceptor(authorizationInterceptor);
	}

	public static void main(String[] args) {
		SpringApplication.run(Wp7Application.class, args);
	}
}

