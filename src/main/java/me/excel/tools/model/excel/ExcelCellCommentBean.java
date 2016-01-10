package me.excel.tools.model.excel;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-17.
 */
public class ExcelCellCommentBean implements ExcelCellComment {

  private ExcelCell excelCell;

  private List<String> comments = new ArrayList<>();

  private int length;

  private int height;

  public ExcelCellCommentBean() {
    this.length = 3;
    this.height = 1;
  }

  public ExcelCellCommentBean(int length, int height) {
    this.length = length;
    this.height = height;
  }

  @Override
  public List<String> getComments() {
    return comments;
  }

  @Override
  public void addComment(String comment) {
    this.comments.add(comment);
  }

  public void setComments(List<String> comments) {
    this.comments = comments;
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
