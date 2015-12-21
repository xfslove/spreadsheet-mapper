package me.excel.tools.model.excel;

/**
 * Created by hanwen on 15-12-17.
 */
public class ExcelCellCommentBean implements ExcelCellComment {

  private ExcelCell excelCell;

  private String comment;

  private int length;

  private int height;

  public ExcelCellCommentBean(String comment) {
    this.comment = comment;
    this.length = 3;
    this.height = 1;
  }

  public ExcelCellCommentBean(String comment, int length, int height) {
    this.comment = comment;
    this.length = length;
    this.height = height;
  }

  public String getComment() {
    return comment;
  }

  @Override
  public int getLength() {
    return length;
  }

  @Override
  public int getHeight() {
    return height;
  }

  @Override
  public ExcelCell getCell() {
    return excelCell;
  }

  @Override
  public ExcelSheet getSheet() {
    return excelCell.getSheet();
  }

  public void setCell(ExcelCell excelCell) {
    this.excelCell = excelCell;
  }

}
