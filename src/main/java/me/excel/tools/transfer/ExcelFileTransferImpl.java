package me.excel.tools.transfer;

import me.excel.tools.model.excel.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * excel 文件到 {@link ExcelWorkbook}的转换器
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileTransferImpl implements ExcelFileTransfer {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelFileTransferImpl.class);

  private ExcelWorkbook excelWorkbook;

  private Map<Integer, String> cellColIndex2field = new HashMap<>();

  @Override
  public ExcelWorkbook transfer(InputStream inputStream) throws IOException {

    excelWorkbook = new ExcelWorkbookBean();

    try {

      if (inputStream.available() == 0) {
        return excelWorkbook;
      }

      Workbook workbook = WorkbookFactory.create(inputStream);

      int sheetCount = workbook.getNumberOfSheets();

      for (int i = 0; i < sheetCount; i++) {

        Sheet sheet = workbook.getSheetAt(i);
        transferSheet(sheet);
        int lastRowNum = sheet.getLastRowNum();

        int maxColNum = 0;
        for (int j = 0; j <= lastRowNum; j++) {

          Row row = sheet.getRow(j);
          transferRow(row);

          if (row != null) {

            maxColNum = row.getLastCellNum() > maxColNum ? row.getLastCellNum() : maxColNum;
            for (int k = 0; k < maxColNum; k++) {

              Cell cell = row.getCell(k);
              transferCell(cell, k, row.getRowNum());
            }
          }
        }
      }

      return excelWorkbook;
    } catch (Exception e) {

      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelTransferException(e);

    } finally {
      inputStream.close();
    }
  }

  private void transferSheet(Sheet sheet) {

    if (sheet == null) {
      return;
    }

    ExcelSheetBean excelSheet = new ExcelSheetBean(sheet);
    excelWorkbook.addSheet(excelSheet);

  }

  private void transferRow(Row row) {

    if (row == null) {
      return;
    }

    ExcelRowBean excelRow = new ExcelRowBean(row);
    ExcelSheetBean lastSheet = (ExcelSheetBean) excelWorkbook.getLastSheet();
    lastSheet.addRow(excelRow);

  }

  private void transferCell(Cell cell, int colIndex, int rowIndex) {

    ExcelSheetBean lastSheet = (ExcelSheetBean) excelWorkbook.getLastSheet();
    ExcelRowBean lastRow = (ExcelRowBean) lastSheet.getLastRow();

    ExcelCellBean excelCell;
    if (cell == null) {
      excelCell = ExcelCellBean.emptyCell(rowIndex + 1, colIndex + 1, cellColIndex2field.get(colIndex));
    } else {

      if (rowIndex == 1) {
        ExcelCell tmpCell = new ExcelCellBean(cell, null);
        cellColIndex2field.put(colIndex, tmpCell.getValue());
      }

      String field = null;
      if (rowIndex != 0 && rowIndex != 1) {
        field = cellColIndex2field.get(colIndex);
      }
      excelCell = new ExcelCellBean(cell, field);

      transferComment(cell, excelCell);
    }

    lastRow.addCell(excelCell);
  }

  private void transferComment(Cell cell, ExcelCell excelCell) {
    Comment cellComment = cell.getCellComment();
    if (cellComment == null) {
      return;
    }
    ExcelCellComment excelCellComment = new ExcelCellCommentBean();
    String commentString = cellComment.getString().getString();
    if (commentString == null) {
      return;
    }
    String[] comments = StringUtils.split(commentString, ",");
    for (String comment : comments) {
      excelCellComment.addComment(comment);
    }
    excelCell.setComment(excelCellComment);
  }

  protected final ExcelWorkbook getExcelWorkbook() {
    return excelWorkbook;
  }
}
