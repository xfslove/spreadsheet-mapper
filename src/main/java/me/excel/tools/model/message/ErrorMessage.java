package me.excel.tools.model.message;

import me.excel.tools.model.excel.ExcelCell;

/**
 * excel validate error message
 *
 * Created by hanwen on 15-12-15.
 */
public class ErrorMessage {

  /**
   * 对应的cell
   */
  private ExcelCell cell;

  private String errorMessage;

  public ErrorMessage(ExcelCell cell, String errorMessage) {
    this.cell = cell;
    this.errorMessage = errorMessage;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public boolean matches(ExcelCell cell) {
    return this.cell.equals(cell);
  }
}
