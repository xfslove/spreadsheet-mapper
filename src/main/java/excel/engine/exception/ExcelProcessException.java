package excel.engine.exception;

/**
 * Created by hanwen on 5/3/16.
 */
public class ExcelProcessException extends RuntimeException {

  public ExcelProcessException() {
    super();
  }

  public ExcelProcessException(String message) {
    super(message);
  }

  public ExcelProcessException(String message, Throwable cause) {
    super(message, cause);
  }

  public ExcelProcessException(Throwable cause) {
    super(cause);
  }

  protected ExcelProcessException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }
}
