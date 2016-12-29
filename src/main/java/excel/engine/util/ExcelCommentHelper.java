package excel.engine.util;

import excel.engine.exception.ExcelWriteException;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.Collection;

/**
 * write comment to excel util
 * <p>
 * Created by hanwen on 15-12-20.
 */
public class ExcelCommentHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelCommentHelper.class);

  private ExcelCommentHelper() {
    // default constructor
  }

  /**
   * write comments
   *
   * @param file     intend write file
   * @param comments comments
   * @see #writeComments(InputStream, OutputStream, Collection)
   */
  public static void writeComments(File file, Collection<excel.engine.model.shapes.Comment> comments) {

    try (Workbook workbook = WorkbookFactory.create(new FileInputStream(file))) {

      addComments(workbook, comments);
      try (FileOutputStream outputStream = new FileOutputStream(file)) {
        workbook.write(outputStream);
      }

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelWriteException(e);
    }
  }

  /**
   * write comments
   *
   * @param inputStream  auto close
   * @param outputStream intend write stream, notice close
   * @param comments     comments
   */
  public static void writeComments(InputStream inputStream, OutputStream outputStream, Collection<excel.engine.model.shapes.Comment> comments) {

    try (Workbook workbook = WorkbookFactory.create(inputStream)) {

      addComments(workbook, comments);
      workbook.write(outputStream);

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelWriteException(e);
    }

  }

  private static void addComments(Workbook workbook, Collection<excel.engine.model.shapes.Comment> comments) {
    int numberOfSheets = workbook.getNumberOfSheets();

    for (excel.engine.model.shapes.Comment comment : comments) {

      if (numberOfSheets < comment.getSheetIndex()) {
        throw new IllegalArgumentException("index of sheet comment at are out of bounds");
      }
      addComment(workbook.getSheetAt(comment.getSheetIndex() - 1), comment);
    }

  }

  private static void addComment(Sheet sheet, excel.engine.model.shapes.Comment comment) {
    if (comment == null) {
      return;
    }
    int rowIndex = comment.getRowIndex();
    int colIndex = comment.getColumnIndex();

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
    anchor.setCol2(colIndex - 1 + comment.getHeight());
    anchor.setRow1(rowIndex - 1);
    anchor.setRow2(rowIndex - 1 + comment.getLength());

    // Create the comment and set the text
    Drawing drawing = sheet.createDrawingPatriarch();
    org.apache.poi.ss.usermodel.Comment poiComment = drawing.createCellComment(anchor);
    RichTextString str = factory.createRichTextString(comment.getMessage());
    poiComment.setString(str);

    // Assign the new comment to the cell
    cell.setCellComment(poiComment);

  }
}
