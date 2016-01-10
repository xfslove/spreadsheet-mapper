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
