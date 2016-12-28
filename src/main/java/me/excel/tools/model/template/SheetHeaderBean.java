package me.excel.tools.model.template;

/**
 * Created by hanwen on 2016/12/27.
 */
public class SheetHeaderBean implements SheetHeader {

  private int sheetIndex;

  private int dataStartRowIndex;

  private boolean hasField;
  private int fieldRowIndex;

  private boolean hasTitle;
  private int titleRowIndex;

  private boolean hasPrompt;
  private int promptRowIndex;

  public SheetHeaderBean(int sheetIndex, int dataStartRowIndex, boolean hasField, int fieldRowIndex, boolean hasTitle, int titleRowIndex, boolean showPrompt, int promptRowIndex) {

    if (dataStartRowIndex <= titleRowIndex || dataStartRowIndex <= fieldRowIndex || dataStartRowIndex <= promptRowIndex) {
      throw new IllegalArgumentException("sheet header error, data row must after title row and field row and prompt row");
    }

    if (titleRowIndex == fieldRowIndex || promptRowIndex == fieldRowIndex || titleRowIndex == promptRowIndex) {
      throw new IllegalArgumentException("sheet header error, row index must difference between title and field and prompt");
    }

    this.sheetIndex = sheetIndex;
    this.dataStartRowIndex = dataStartRowIndex;
    this.hasField = hasField;
    this.fieldRowIndex = fieldRowIndex;
    this.hasTitle = hasTitle;
    this.titleRowIndex = titleRowIndex;
    this.hasPrompt = showPrompt;
    this.promptRowIndex = promptRowIndex;
  }

  /**
   * @param sheetIndex which sheet use default header
   * @return header info
   * @see SheetHeader
   */
  public static SheetHeaderBean DEFAULT(int sheetIndex) {
    return new SheetHeaderBean(sheetIndex, 4, true, 2, true, 1, true, 3);
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  public int getDataStartRowIndex() {
    return dataStartRowIndex;
  }

  public boolean isHasField() {
    return hasField;
  }

  public int getFieldRowIndex() {
    return fieldRowIndex;
  }

  public boolean isHasTitle() {
    return hasTitle;
  }

  public int getTitleRowIndex() {
    return titleRowIndex;
  }

  public boolean isHasPrompt() {
    return hasPrompt;
  }

  public int getPromptRowIndex() {
    return promptRowIndex;
  }
}
