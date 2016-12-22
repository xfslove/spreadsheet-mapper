package me.excel.tools.validator;

/**
 * <pre>
 * like {@link java.util.function.Function}, but this can throws {@link SkipValidateException}.
 * this useful when validate cell value,
 * when validate A cell, result is failure, but the reason caused by B cell,
 * then can throws this exception with B cell & reason.
 * the {@link UserFileValidator} will handle this exception to collect the error message on
 * correct cell.
 * </pre>
 * Created by hanwen on 16/3/31.
 */
@FunctionalInterface
public interface ValidateFunction<T, R> {

  R apply(T t) throws SkipValidateException;

}
