package me.excel.tools.exception;

/**
 * Created by hanwen on 2016/12/28.
 */
public class ExcelValidateException extends RuntimeException {

  public ExcelValidateException() {
    super();
  }

  public ExcelValidateException(String message) {
    super(message);
  }

  public ExcelValidateException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelValidateException(Throwable cause) {
    super(cause);
  }

  protected ExcelValidateException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
