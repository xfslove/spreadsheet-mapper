package me.excel.tools.transfer;

import me.excel.tools.model.excel.*;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

/**
 * excel 文件到 {@link ExcelWorkbook}的转换器
 *
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileTransfer implements UserFileTransfer {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelFileTransfer.class);

  protected ExcelWorkbook excelWorkbook;

  protected Map<Integer, String> cellColIndex2field = new HashMap<>();

  /**
   * 将 inputStream 写入 {@link ExcelWorkbook}
   *
   * @param inputStream 会自动关闭
   */
  @Override
  public void transfer(InputStream inputStream) {

    try (Workbook workbook = WorkbookFactory.create(inputStream)) {

      excelWorkbook = new ExcelWorkbookBean();
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
              transferCell(cell);
            }
          }
        }
      }

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new RuntimeException(e);
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

  private void transferCell(Cell cell) {

    if (cell == null) {
      return;
    }

    int colIndex = cell.getColumnIndex();
    if (cell.getRowIndex() == 1) {
      ExcelCell tmpCell = new ExcelCellBean(cell, null);
      cellColIndex2field.put(colIndex, tmpCell.getValue());
    }

    ExcelSheetBean lastSheet = (ExcelSheetBean) excelWorkbook.getLastSheet();
    ExcelRowBean lastRow = (ExcelRowBean) lastSheet.getLastRow();

    ExcelCellBean excelCell;
    if (cell == null) {
      excelCell = ExcelCellBean.emptyCell(cell.getRowIndex() + 1, colIndex + 1, cellColIndex2field.get(colIndex));
    } else {
      String field = null;
      if (cell.getRowIndex() != 0 && cell.getRowIndex() != 1 && cell.getRowIndex() != 2) {
        field = cellColIndex2field.get(colIndex);
      }
      excelCell = new ExcelCellBean(cell, field);
    }
    lastRow.addCell(excelCell);

    transferComment(cell, excelCell);
  }

  private void transferComment(Cell cell, ExcelCell excelCell) {
    Comment cellComment = cell.getCellComment();
    if (cellComment == null) {
      return;
    }
    excelCell.setComment(new ExcelCellCommentBean(cellComment.getString().getString()));
  }


}
