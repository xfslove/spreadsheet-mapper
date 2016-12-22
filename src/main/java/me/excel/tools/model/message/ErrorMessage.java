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
   * error content
   */
  private String content;

  public ErrorMessage(ExcelCell cell, String content) {
    this.cell = cell;
    this.content = content;
  }

  public String getContent() {
    return content;
  }

  public ExcelCell getCell() {
    return cell;
  }
}
