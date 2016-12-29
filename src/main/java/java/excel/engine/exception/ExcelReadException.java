package java.excel.engine.exception;

/**
 * Created by hanwen on 16/1/7.
 */
public class ExcelReadException extends RuntimeException {

  public ExcelReadException() {
    super();
  }

  public ExcelReadException(String message) {
    super(message);
  }

  public ExcelReadException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelReadException(Throwable cause) {
    super(cause);
  }

  public ExcelReadException(String message, Throwable cause, boolean enableSuppression,
                            boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
