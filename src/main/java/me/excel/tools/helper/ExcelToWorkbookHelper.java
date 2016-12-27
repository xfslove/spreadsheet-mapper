package me.excel.tools.helper;

import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.model.excel.*;
import me.excel.tools.model.template.ExcelSheetHeaderInfo;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ExcelToWorkbookHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelToWorkbookHelper.class);

  private ExcelToWorkbookHelper() {
    // default constructor
  }

  /**
   * read supplied excel stream to {@link ExcelWorkbook}
   *
   * @param inputStream auto close
   * @return workbook
   * @throws IOException io exception
   */
  public static ExcelWorkbook read(InputStream inputStream, ExcelSheetHeaderInfo... headerInfos) throws IOException {

    ExcelWorkbook excelWorkbook = new ExcelWorkbookBean();

    try {

      if (inputStream.available() == 0) {
        return excelWorkbook;
      }

      Workbook workbook = WorkbookFactory.create(inputStream);

      if (workbook instanceof HSSFWorkbook) {
        excelWorkbook.setAfter97(false);
      }

      int sheetCount = workbook.getNumberOfSheets();

      Map<Integer, ExcelSheetHeaderInfo> sheetIndex2headerInfo = buildHeaderInfoMap(headerInfos);
      for (int i = 0; i < sheetCount; i++) {

        Sheet sheet = workbook.getSheetAt(i);

        if (sheet == null) {
          continue;
        }

        ExcelSheet excelSheet = transferSheet(sheet);
        ExcelSheetHeaderInfo headerInfo = sheetIndex2headerInfo.get(excelSheet.getIndex());
        if (headerInfo != null) {
          excelSheet.setHeaderInfo(headerInfo);
        }
        excelWorkbook.addSheet(excelSheet);

        Map<Integer, String> columnIndex2field = buildColumnIndex2fieldMap(sheet.getRow(excelSheet.getHeaderInfo().getFieldAt() - 1));

        int lastRowNum = sheet.getLastRowNum();
        for (int j = 0; j <= lastRowNum; j++) {

          Row row = sheet.getRow(j);

          ExcelRow excelRow = transferRow(row);
          excelSheet.addRow(excelRow);

          List<ExcelCell> excelCells = transferRowCells(row);

          for (ExcelCell excelCell : excelCells) {

            excelCell.setField(columnIndex2field.get(excelCell.getColumnIndex()));
            excelRow.addCell(excelCell);
          }

        }
      }

      return excelWorkbook;
    } catch (Exception e) {

      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelReadException(e);

    } finally {
      inputStream.close();
    }
  }

  private static ExcelSheet transferSheet(Sheet sheet) {
    return new ExcelSheetBean(sheet);
  }

  private static ExcelRow transferRow(Row row) {
    return new ExcelRowBean(row);
  }

  private static List<ExcelCell> transferRowCells(Row row) {
    List<ExcelCell> excelCells = new ArrayList<>();

    int maxColNum = row.getLastCellNum() > 0 ? row.getLastCellNum() : 0;

    for (int k = 0; k < maxColNum; k++) {

      Cell cell = row.getCell(k);

      ExcelCellBean excelCell;
      if (cell == null) {

        excelCell = ExcelCellBean.EMPTY_CELL(k + 1, row.getRowNum() + 1);
      } else {

        excelCell = new ExcelCellBean(cell);
      }

      excelCells.add(excelCell);
    }

    return excelCells;
  }

  private static Map<Integer, ExcelSheetHeaderInfo> buildHeaderInfoMap(ExcelSheetHeaderInfo[] sheetHeaderInfos) {
    Map<Integer, ExcelSheetHeaderInfo> sheetIndex2headerInfo = new HashMap<>();

    if (sheetHeaderInfos != null) {
      for (ExcelSheetHeaderInfo headerInfo : sheetHeaderInfos) {
        sheetIndex2headerInfo.put(headerInfo.getSheetIndex(), headerInfo);
      }
    }

    return sheetIndex2headerInfo;
  }

  private static Map<Integer, String> buildColumnIndex2fieldMap(Row row) {
    Map<Integer, String> columnIndex2field = new HashMap<>();

    List<ExcelCell> excelCells = transferRowCells(row);

    for (ExcelCell excelCell : excelCells) {
      columnIndex2field.put(excelCell.getColumnIndex(), excelCell.getValue());
    }

    return columnIndex2field;
  }

}
