package me.excel.tools.model.extra;

/**
 * Created by hanwen on 2016/12/27.
 */
public class ExcelTextBoxBean implements ExcelTextBox {

  private String message;

  private int sheetIndex;

  private TextBoxStyle style;

  public ExcelTextBoxBean(int sheetIndex, String message) {
    this.sheetIndex = sheetIndex;
    this.message = message;
    this.style = TextBoxStyle.DEFAULT;
  }

  public ExcelTextBoxBean(String message, int sheetIndex, TextBoxStyle style) {
    this.message = message;
    this.sheetIndex = sheetIndex;
    this.style = style;
  }

  @Override
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public String getMessage() {
    return message;
  }

  @Override
  public TextBoxStyle getStyle() {
    return style;
  }
}
