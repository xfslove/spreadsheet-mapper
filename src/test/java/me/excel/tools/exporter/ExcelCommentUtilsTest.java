package me.excel.tools.exporter;

import me.excel.tools.model.excel.*;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 15-12-21.
 */
public class ExcelCommentUtilsTest {
  
  @Test
  public void testWriteToFile() throws Exception {
    ExcelWorkbook excelWorkbook = new ExcelWorkbookBean();

    ExcelSheet excelSheet = new ExcelSheetBean();
    excelWorkbook.addSheet(excelSheet);

    ExcelRowBean excelRow1 = new ExcelRowBean(1);
    excelSheet.addRow(excelRow1);
    ExcelCellBean excelCell = new ExcelCellBean(1, 1, "student.code", "111111");
    excelRow1.addCell(excelCell);

    excelCell.setComment(new ExcelCellCommentBean("student\'s code"));

    File file = new File("/home/hanwen/tmp/test.xlsx");

    List<ExcelCellComment> commentList = new ArrayList<>();
    commentList.add(excelCell.getComment());
    ExcelCommentUtils.writeToFile(file, commentList);
  }
}