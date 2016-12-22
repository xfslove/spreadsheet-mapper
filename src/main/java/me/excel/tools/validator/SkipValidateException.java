package me.excel.tools.validator;


import me.excel.tools.model.excel.ExcelCell;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * use to skip same validate
 * <p>
 * Created by hanwen on 16/3/18.
 */
public class SkipValidateException extends Exception {

  /**
   * write error message on which cells
   */
  protected List<ExcelCell> cells = new ArrayList<>();

  /**
   * error message
   */
  protected String errorMessage;

  public SkipValidateException(ExcelCell... cells) {
    this.errorMessage = "数据错误";

    if (cells != null) {

      Collections.addAll(this.cells, cells);
    }
  }

  public SkipValidateException(String errorMessage, ExcelCell... cells) {
    this.errorMessage = errorMessage;
    if (cells != null) {

      Collections.addAll(this.cells, cells);
    }
  }

  public SkipValidateException(String message, String errorMessage, ExcelCell... cells) {
    super(message);
    this.errorMessage = errorMessage;
    if (cells != null) {

      Collections.addAll(this.cells, cells);
    }
  }

  public SkipValidateException(String message, Throwable cause, String errorMessage, ExcelCell... cells) {
    super(message, cause);
    this.errorMessage = errorMessage;
    if (cells != null) {

      Collections.addAll(this.cells, cells);
    }
  }

  public SkipValidateException(Throwable cause, String errorMessage, ExcelCell... cells) {
    super(cause);
    this.errorMessage = errorMessage;
    if (cells != null) {

      Collections.addAll(this.cells, cells);
    }
  }

  public SkipValidateException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String errorMessage, ExcelCell... cells) {
    super(message, cause, enableSuppression, writableStackTrace);
    this.errorMessage = errorMessage;

    if (cells != null) {
      Collections.addAll(this.cells, cells);
    }
  }

  public List<ExcelCell> getCells() {
    return cells;
  }

  public String getErrorMessage() {
    return errorMessage;
  }
}
