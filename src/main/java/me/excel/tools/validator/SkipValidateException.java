package me.excel.tools.validator;

import me.excel.tools.model.excel.ExcelCell;

/**
 * 用于跳过某些校验的exception
 * Created by hanwen on 16/3/18.
 */
public class SkipValidateException extends Exception {

  /**
   * 因为哪个cell跳过
   */
  protected ExcelCell cell;

  /**
   * 错误消息
   */
  protected String prompt;

  public SkipValidateException(ExcelCell cell) {
    this.cell = cell;
    this.prompt = "数据错误";
  }

  public SkipValidateException(ExcelCell cell, String prompt) {
    this.cell = cell;
    this.prompt = prompt;
  }

  public SkipValidateException(String message, ExcelCell cell, String prompt) {
    super(message);
    this.cell = cell;
    this.prompt = prompt;
  }

  public SkipValidateException(String message, Throwable cause, ExcelCell cell, String prompt) {
    super(message, cause);
    this.cell = cell;
    this.prompt = prompt;
  }

  public SkipValidateException(Throwable cause, ExcelCell cell, String prompt) {
    super(cause);
    this.cell = cell;
    this.prompt = prompt;
  }

  public SkipValidateException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, ExcelCell cell, String prompt) {
    super(message, cause, enableSuppression, writableStackTrace);
    this.cell = cell;
    this.prompt = prompt;
  }

  public ExcelCell getCell() {
    return cell;
  }

  public String getPrompt() {
    return prompt;
  }
}
