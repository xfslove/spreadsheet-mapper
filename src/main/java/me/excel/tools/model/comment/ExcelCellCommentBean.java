package me.excel.tools.model.comment;

import me.excel.tools.model.excel.ExcelCell;

/**
 * Created by hanwen on 15-12-17.
 */
public class ExcelCellCommentBean implements ExcelCellComment {

  private int sheetIndex;

  private int rowNum;

  private int columnNum;

  private int length;

  private int height;

  private String comment;

  public ExcelCellCommentBean() {
    this.length = 3;
    this.height = 1;
  }

  public ExcelCellCommentBean(int length, int height) {
    this.length = length;
    this.height = height;
  }

  @Override
  public String getComment() {
    return comment;
  }

  @Override
  public void setComment(String comment) {
    this.comment = comment;
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
  public int getSheetIndex() {
    return sheetIndex;
  }

  @Override
  public int getRowNum() {
    return rowNum;
  }

  @Override
  public int getColumnNum() {
    return columnNum;
  }

  @Override
  public void setSheetIndex(int sheetIndex) {
    this.sheetIndex = sheetIndex;
  }

  @Override
  public void setRowNum(int rowNum) {
    this.rowNum = rowNum;
  }

  @Override
  public void setColumnNum(int columnNum) {
    this.columnNum = columnNum;
  }

  @Override
  public void setCell(ExcelCell cell) {
    this.sheetIndex = cell.getSheet().getIndex();
    this.rowNum = cell.getRowNum();
    this.columnNum = cell.getColumnNum();
  }
}
