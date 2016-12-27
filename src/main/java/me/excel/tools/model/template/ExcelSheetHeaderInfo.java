package me.excel.tools.model.template;

import java.io.Serializable;

/**
 * <pre>
 * sheet header information include:
 * 1. which row is field row (this is importance, is determined cell value of this column corresponding which field of object)
 * 2. data start at which row
 * 3. which row is title (may empty)
 * 4. which row is prompt (may empty)
 * </pre>
 * Created by hanwen on 2016/12/27.
 */
public class ExcelSheetHeaderInfo implements Serializable {

  public static final ExcelSheetHeaderInfo SINGLE_SHEET_DEFAULT = new ExcelSheetHeaderInfo(1, 2, 4, true, 1, true, 3);

  private int sheetIndex;

  private int fieldAt;

  private int dataStartAt;

  private boolean hasTitle;
  private int titleAt;

  private boolean hasPrompt;
  private int promptAt;

  public ExcelSheetHeaderInfo(int sheetIndex, int fieldAt, int dataStartAt, boolean hasTitle, int titleAt, boolean hasPrompt, int promptAt) {
    this.sheetIndex = sheetIndex;
    this.fieldAt = fieldAt;
    this.dataStartAt = dataStartAt;
    this.hasTitle = hasTitle;
    this.titleAt = titleAt;
    this.hasPrompt = hasPrompt;
    this.promptAt = promptAt;
  }

  public int getSheetIndex() {
    return sheetIndex;
  }

  public int getFieldAt() {
    return fieldAt;
  }

  public int getDataStartAt() {
    return dataStartAt;
  }

  public boolean isHasTitle() {
    return hasTitle;
  }

  public int getTitleAt() {
    return titleAt;
  }

  public boolean isHasPrompt() {
    return hasPrompt;
  }

  public int getPromptAt() {
    return promptAt;
  }
}
