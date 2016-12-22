package me.excel.tools.exporter;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.comment.ExcelCellComment;
import me.excel.tools.model.comment.ExcelCellCommentBean;
import me.excel.tools.model.excel.*;
import org.apache.poi.util.TempFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 15-12-20.
 */
public class ExcelFileExporterTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelFileExporterTest.class);

  private File excel;

  @BeforeTest
  public void init() throws IOException {
    excel = TempFile.createTempFile("test", ExcelConstants.SUFFIX_XLSX);
    LOGGER.info(excel.getAbsolutePath());
  }

  @Test
  public void testExport() throws Exception {

    ExcelWorkbook excelWorkbook = new ExcelWorkbookBean();

    ExcelSheet excelSheet = new ExcelSheetBean();
    excelWorkbook.addSheet(excelSheet);

    ExcelRowBean excelRow1 = new ExcelRowBean(1);
    excelSheet.addRow(excelRow1);
    excelRow1.addCell(new ExcelCellBean(1, 1, "student.code", "111111"));
    excelRow1.addCell(new ExcelCellBean(1, 2, "student.name", "std1"));
    excelRow1.addCell(new ExcelCellBean(1, 3, "student.age", "18"));
    excelRow1.addCell(new ExcelCellBean(1, 4, "student.enrollDate", "2015-09-01"));

    ExcelRowBean excelRow2 = new ExcelRowBean(2);
    excelRow2.addCell(new ExcelCellBean(1, 1, "student.code", "2222"));
    excelRow2.addCell(new ExcelCellBean(1, 2, "student.name", "std2"));
    excelRow2.addCell(new ExcelCellBean(1, 3, "student.age", "18"));
    excelRow2.addCell(new ExcelCellBean(1, 4, "student.enrollDate", "2015-09-01"));
    excelSheet.addRow(excelRow2);

    UserFileExporter fileExporter = new ExcelFileExporter(excelWorkbook);

    fileExporter.export(new FileOutputStream(excel));
  }

  @Test(dependsOnMethods = "testExport")
  public void testWriteComments() throws Exception {

    ExcelCellCommentBean excelCellComment = new ExcelCellCommentBean();
    excelCellComment.setComment("test comment1");
    excelCellComment.setSheetIndex(1);
    excelCellComment.setRowNum(1);
    excelCellComment.setColumnNum(1);

    List<ExcelCellComment> commentList = new ArrayList<>();
    commentList.add(excelCellComment);
    ExcelCommentUtils.writeComments(excel, commentList);
  }
}