package me.excel.tools.transfer;

/**
 * Created by hanwen on 16/1/7.
 */
public class ExcelTransferException extends RuntimeException {

  public ExcelTransferException() {
    super();
  }

  public ExcelTransferException(String message) {
    super(message);
  }

  public ExcelTransferException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelTransferException(Throwable cause) {
    super(cause);
  }

  public ExcelTransferException(String message, Throwable cause, boolean enableSuppression,
                                boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
