package excel.engine.w2f;

import excel.engine.model.core.*;
import excel.engine.util.ExcelCommentHelper;
import excel.engine.ExcelConstants;
import excel.engine.model.shapes.Comment;
import excel.engine.model.shapes.CommentBean;
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
public class HSSFWorkbook2ExcelWriterTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(HSSFWorkbook2ExcelWriterTest.class);

  private File excel;

  @BeforeTest
  public void init() throws IOException {
    excel = TempFile.createTempFile("test", ExcelConstants.SUFFIX_XLSX);
    LOGGER.info(excel.getAbsolutePath());
  }

  @Test
  public void testExport() throws Exception {

    Workbook workbook = new WorkbookBean();

    Sheet sheet = new SheetBean();
    workbook.addSheet(sheet);

    RowBean excelRow1 = new RowBean(1);
    sheet.addRow(excelRow1);
    excelRow1.addCell(new CellBean(1, 1, "111111"));
    excelRow1.addCell(new CellBean(1, 2, "std1"));
    excelRow1.addCell(new CellBean(1, 3, "18"));
    excelRow1.addCell(new CellBean(1, 4, "2015-09-01"));

    RowBean excelRow2 = new RowBean(2);
    excelRow2.addCell(new CellBean(1, 1, "2222"));
    excelRow2.addCell(new CellBean(1, 2, "std2"));
    excelRow2.addCell(new CellBean(1, 3, "18"));
    excelRow2.addCell(new CellBean(1, 4, "2015-09-01"));
    sheet.addRow(excelRow2);

    WorkbookWriter writer = new HSSFWorkbook2ExcelWriter();
    writer.write(workbook, new FileOutputStream(excel));
  }

  @Test(dependsOnMethods = "testExport")
  public void testWriteComments() throws Exception {

    CommentBean excelCellComment = new CommentBean("test comment1", 1, 1, 1);
    List<Comment> commentList = new ArrayList<>();
    commentList.add(excelCellComment);
    ExcelCommentHelper.writeComments(excel, commentList);
  }
}