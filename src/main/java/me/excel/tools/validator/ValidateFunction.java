package me.excel.tools.validator;

/**
 * Created by hanwen on 16/4/2.
 */
@FunctionalInterface
public interface ValidateFunction<T, R> {

    R apply(T t) throws SkipValidateException;
}
