package me.excel.tools.exporter;

import me.excel.tools.model.excel.ExcelCellComment;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.List;

/**
 * write comment extractor
 *
 * Created by hanwen on 15-12-20.
 */
public abstract class ExcelCommentUtils {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelCommentUtils.class);

  private ExcelCommentUtils() {

  }

  /**
   * 将 comments 写到 excel 上
   *
   * @param excel
   * @param commentList
   */
  public static void writeToFile(File excel, List<ExcelCellComment> commentList) {

    try (Workbook workbook = WorkbookFactory.create(new FileInputStream(excel))) {

      int numberOfSheets = workbook.getNumberOfSheets();

      commentList.forEach(excelComment -> {
        int sheetIndex = excelComment.getSheet().getIndex();
        if (numberOfSheets < sheetIndex) {
          throw new IllegalArgumentException("index of sheet comment at are out of bounds");
        }
        addCommentOnSheet(workbook.getSheetAt(sheetIndex - 1), excelComment);
      });

      try (OutputStream out = new FileOutputStream(excel)) {
        workbook.write(out);
      }
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelExportException(e);
    }

  }

  public static void addCommentOnSheet(Sheet sheet, ExcelCellComment excelCellComment) {
    if (excelCellComment == null) {
      return;
    }
    int rowIndex = excelCellComment.getCell().getRowNum();
    int colIndex = excelCellComment.getCell().getColumnNum();

    Row row = sheet.getRow(rowIndex - 1);
    Cell cell = row.getCell(colIndex - 1);
    if (cell == null) {
      cell = row.createCell(colIndex - 1, Cell.CELL_TYPE_STRING);
    }
    // remove the old comment
    cell.removeCellComment();

    CreationHelper factory = sheet.getWorkbook().getCreationHelper();
    ClientAnchor anchor = factory.createClientAnchor();
    // When the comment box is visible, have it show in a 1x3 space
    anchor.setCol1(colIndex - 1);
    anchor.setCol2(colIndex - 1 + excelCellComment.getHeight());
    anchor.setRow1(rowIndex - 1);
    anchor.setRow2(rowIndex - 1 + excelCellComment.getLength());

    // Create the comment and set the text
    Drawing drawing = sheet.createDrawingPatriarch();
    Comment comment = drawing.createCellComment(anchor);
    RichTextString str = factory.createRichTextString(StringUtils.join(excelCellComment.getComments(), ","));
    comment.setString(str);

    // Assign the new comment to the cell
    cell.setCellComment(comment);

  }
}
