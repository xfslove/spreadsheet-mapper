package me.excel.tools.validator;

/**
 * Created by hanwen on 16/3/24.
 */
@FunctionalInterface
public interface ValidateFunction<T, R> {

  R apply(T t) throws SkipValidateException;

}
