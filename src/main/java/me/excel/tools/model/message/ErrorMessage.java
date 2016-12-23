package me.excel.tools.model.message;

import me.excel.tools.model.excel.ExcelCell;

import java.io.Serializable;

/**
 * excel validate error message
 * <p>
 * Created by hanwen on 15-12-15.
 */
public class ErrorMessage implements Serializable {

  /**
   * belong cell
   */
  private ExcelCell cell;

  /**
   * error message
   */
  private String errorMessage;

  public ErrorMessage(ExcelCell cell, String errorMessage) {
    this.cell = cell;
    this.errorMessage = errorMessage;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public ExcelCell getCell() {
    return cell;
  }
}
