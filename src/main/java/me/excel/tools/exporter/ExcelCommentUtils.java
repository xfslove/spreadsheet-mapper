package me.excel.tools.exporter;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.comment.ExcelCellComment;
import me.excel.tools.model.comment.ExcelCellCommentBean;
import me.excel.tools.model.message.ErrorMessage;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * write comment to excel util
 * <p>
 * Created by hanwen on 15-12-20.
 */
public abstract class ExcelCommentUtils {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelCommentUtils.class);

  private ExcelCommentUtils() {
    // default constructor
  }

  /**
   * @param errorMessages
   * @return
   * @see #transferErrorMessagesToComments(Collection, int, int)
   */
  public static List<ExcelCellComment> transferErrorMessagesToComments(Collection<ErrorMessage> errorMessages) {

    Map<Integer, Map<Integer, Map<Integer, List<String>>>> errorMessageMap =
        errorMessages.stream().collect(
            Collectors.groupingBy(e -> e.getCell().getSheet().getIndex(),
                Collectors.groupingBy(e -> e.getCell().getRowNum(),
                    Collectors.groupingBy(e -> e.getCell().getColumnNum(),
                        Collectors.collectingAndThen(Collectors.toList(), e -> e.stream().map(ErrorMessage::getErrorMessage).collect(Collectors.toList()))
                    )
                )
            )
        );

    List<ExcelCellComment> commentList = new ArrayList<>();
    for (Map.Entry<Integer, Map<Integer, Map<Integer, List<String>>>> sheet : errorMessageMap.entrySet()) {
      for (Map.Entry<Integer, Map<Integer, List<String>>> row : sheet.getValue().entrySet()) {
        for (Map.Entry<Integer, List<String>> column : row.getValue().entrySet()) {
          ExcelCellComment comment = new ExcelCellCommentBean();
          comment.setSheetIndex(sheet.getKey());
          comment.setRowNum(row.getKey());
          comment.setColumnNum(column.getKey());
          comment.setComment(StringUtils.join(column.getValue(), ExcelConstants.SEPARATOR));
          commentList.add(comment);
        }
      }
    }

    return commentList;
  }

  /**
   * transfer validate error messages {@link ErrorMessage} to comment {@link ExcelCellComment}
   *
   * @param errorMessages
   * @param commentLength
   * @param commentHeight
   * @return
   */
  public static List<ExcelCellComment> transferErrorMessagesToComments(Collection<ErrorMessage> errorMessages, int commentLength, int commentHeight) {

    Map<Integer, Map<Integer, Map<Integer, List<String>>>> errorMessageMap =
        errorMessages.stream().collect(
            Collectors.groupingBy(e -> e.getCell().getSheet().getIndex(),
                Collectors.groupingBy(e -> e.getCell().getRowNum(),
                    Collectors.groupingBy(e -> e.getCell().getColumnNum(),
                        Collectors.collectingAndThen(Collectors.toList(), e -> e.stream().map(ErrorMessage::getErrorMessage).collect(Collectors.toList()))
                    )
                )
            )
        );

    List<ExcelCellComment> commentList = new ArrayList<>();
    for (Map.Entry<Integer, Map<Integer, Map<Integer, List<String>>>> sheet : errorMessageMap.entrySet()) {
      for (Map.Entry<Integer, Map<Integer, List<String>>> row : sheet.getValue().entrySet()) {
        for (Map.Entry<Integer, List<String>> column : row.getValue().entrySet()) {
          ExcelCellComment comment = new ExcelCellCommentBean(commentLength, commentHeight);
          comment.setSheetIndex(sheet.getKey());
          comment.setRowNum(row.getKey());
          comment.setColumnNum(column.getKey());
          comment.setComment(StringUtils.join(column.getValue(), ExcelConstants.SEPARATOR));
          commentList.add(comment);
        }
      }
    }

    return commentList;
  }

  /**
   * write comments
   *
   * @param file
   * @param comments
   * @see #writeComments(InputStream, OutputStream, Collection)
   */
  public static void writeComments(File file, Collection<ExcelCellComment> comments) {

    try (Workbook workbook = WorkbookFactory.create(new FileInputStream(file))) {

      int numberOfSheets = workbook.getNumberOfSheets();

      comments.forEach(excelComment -> {
        if (numberOfSheets < excelComment.getSheetIndex()) {
          throw new IllegalArgumentException("index of sheet comment at are out of bounds");
        }
        addComment(workbook.getSheetAt(excelComment.getSheetIndex() - 1), excelComment);
      });

      try (FileOutputStream outputStream = new FileOutputStream(file)) {
        workbook.write(outputStream);
      }

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelExportException(e);
    }
  }

  /**
   * write comments
   *
   * @param inputStream  auto close
   * @param outputStream
   * @param comments
   */
  public static void writeComments(InputStream inputStream, OutputStream outputStream, Collection<ExcelCellComment> comments) {

    try (Workbook workbook = WorkbookFactory.create(inputStream)) {

      int numberOfSheets = workbook.getNumberOfSheets();

      comments.forEach(excelComment -> {
        if (numberOfSheets < excelComment.getSheetIndex()) {
          throw new IllegalArgumentException("index of sheet comment at are out of bounds");
        }
        addComment(workbook.getSheetAt(excelComment.getSheetIndex() - 1), excelComment);
      });

      workbook.write(outputStream);

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelExportException(e);
    }

  }

  /**
   * add one cell comment
   *
   * @param sheet
   * @param excelCellComment
   */
  public static void addComment(Sheet sheet, ExcelCellComment excelCellComment) {
    if (excelCellComment == null) {
      return;
    }
    int rowIndex = excelCellComment.getRowNum();
    int colIndex = excelCellComment.getColumnNum();

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
    RichTextString str = factory.createRichTextString(excelCellComment.getComment());
    comment.setString(str);

    // Assign the new comment to the cell
    cell.setCellComment(comment);

  }
}
