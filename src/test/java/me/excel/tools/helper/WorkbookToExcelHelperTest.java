package me.excel.tools.helper;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.extra.ExcelComment;
import me.excel.tools.model.extra.ExcelCommentBean;
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
public class WorkbookToExcelHelperTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(WorkbookToExcelHelperTest.class);

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
    excelRow1.addCell(new ExcelCellBean(1, 1, "111111"));
    excelRow1.addCell(new ExcelCellBean(1, 2, "std1"));
    excelRow1.addCell(new ExcelCellBean(1, 3, "18"));
    excelRow1.addCell(new ExcelCellBean(1, 4, "2015-09-01"));

    ExcelRowBean excelRow2 = new ExcelRowBean(2);
    excelRow2.addCell(new ExcelCellBean(1, 1, "2222"));
    excelRow2.addCell(new ExcelCellBean(1, 2, "std2"));
    excelRow2.addCell(new ExcelCellBean(1, 3, "18"));
    excelRow2.addCell(new ExcelCellBean(1, 4, "2015-09-01"));
    excelSheet.addRow(excelRow2);

    WorkbookToExcelHelper.write(excelWorkbook, new FileOutputStream(excel));
  }

  @Test(dependsOnMethods = "testExport")
  public void testWriteComments() throws Exception {

    ExcelCommentBean excelCellComment = new ExcelCommentBean("test comment1", 1, 1, 1);
    List<ExcelComment> commentList = new ArrayList<>();
    commentList.add(excelCellComment);
    ExcelCommentHelper.writeComments(excel, commentList);
  }
}