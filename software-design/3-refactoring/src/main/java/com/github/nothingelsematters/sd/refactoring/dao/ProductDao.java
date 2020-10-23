package com.github.nothingelsematters.sd.refactoring.dao;

import com.github.nothingelsematters.sd.refactoring.entity.Product;

import java.util.List;
import java.util.Optional;

public interface ProductDao {

    void initialize();

    void insert(Product product);
    
    List<Product> getAll();
    
    Optional<Product> getMostExpensive();
    
    Optional<Product> getLessExpensive();

    int getPriceSum();

    int getAmount();
}
