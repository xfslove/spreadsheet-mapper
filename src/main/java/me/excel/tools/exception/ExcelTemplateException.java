package me.excel.tools.exception;

/**
 * Created by hanwen on 2016/12/29.
 */
public class ExcelTemplateException extends RuntimeException {

  public ExcelTemplateException() {
    super();
  }

  public ExcelTemplateException(String message) {
    super(message);
  }

  public ExcelTemplateException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelTemplateException(Throwable cause) {
    super(cause);
  }

  protected ExcelTemplateException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
