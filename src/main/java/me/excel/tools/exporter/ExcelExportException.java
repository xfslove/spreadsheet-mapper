package me.excel.tools.exporter;

/**
 * Created by hanwen on 16/1/7.
 */
public class ExcelExportException extends RuntimeException {
  public ExcelExportException() {
    super();
  }

  public ExcelExportException(String message) {
    super(message);
  }

  public ExcelExportException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelExportException(Throwable cause) {
    super(cause);
  }

  protected ExcelExportException(String message, Throwable cause, boolean enableSuppression,
                                 boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
