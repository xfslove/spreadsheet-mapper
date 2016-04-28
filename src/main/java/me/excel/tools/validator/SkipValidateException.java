package me.excel.tools.validator;


import me.excel.tools.model.excel.ExcelCell;

import java.util.ArrayList;
import java.util.List;

/**
 * 用于跳过某些校验的exception
 * Created by hanwen on 16/3/18.
 */
public class SkipValidateException extends Exception {

  /**
   * 将错误信息写到哪些cell上
   */
  protected List<ExcelCell> cells = new ArrayList<>();

  /**
   * 错误消息
   */
  protected String prompt;

  public SkipValidateException(ExcelCell... cells) {
    this.prompt = "数据错误";

    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public SkipValidateException(String prompt, ExcelCell... cells) {
    this.prompt = prompt;
    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public SkipValidateException(String message, String prompt, ExcelCell... cells) {
    super(message);
    this.prompt = prompt;
    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public SkipValidateException(String message, Throwable cause, String prompt, ExcelCell... cells) {
    super(message, cause);
    this.prompt = prompt;
    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public SkipValidateException(Throwable cause, String prompt, ExcelCell... cells) {
    super(cause);
    this.prompt = prompt;
    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public SkipValidateException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, String prompt, ExcelCell... cells) {
    super(message, cause, enableSuppression, writableStackTrace);
    this.prompt = prompt;
    if (cells != null) {

      for (ExcelCell cell : cells) {
        this.cells.add(cell);
      }
    }
  }

  public List<ExcelCell> getCells() {
    return cells;
  }

  public String getPrompt() {
    return prompt;
  }
}
