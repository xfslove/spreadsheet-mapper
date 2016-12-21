package me.excel.tools.factory;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelCellComment;
import me.excel.tools.model.excel.ExcelCellCommentBean;
import me.excel.tools.model.message.ErrorMessage;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2016/12/21.
 */
public class TestClass {

  public static void main(String[] args) {

    List<ErrorMessage> errorMessages = new ArrayList<>();

    List<ExcelCellComment> commentList = new ArrayList<>();
    errorMessages.forEach(errorMessage -> {
      ExcelCell excelCell = errorMessage.getCell();
      ExcelCellComment excelCellComment;
      if (excelCell.getComment() == null) {
        excelCellComment = new ExcelCellCommentBean();
        excelCell.setComment(excelCellComment);
      }
      excelCellComment = excelCell.getComment();
      excelCellComment.addComment(errorMessage.getContent());
      commentList.add(excelCellComment);
    });

  }
}
