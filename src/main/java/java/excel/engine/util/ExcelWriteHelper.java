package java.excel.engine.util;

import java.excel.engine.ExcelConstants;
import java.excel.engine.exception.ExcelWriteException;
import java.excel.engine.model.excel.Cell;
import java.excel.engine.model.excel.Row;
import java.excel.engine.model.excel.Sheet;
import java.excel.engine.model.excel.Workbook;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.OutputStream;

/**
 * workbook to excel file exporter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class ExcelWriteHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelWriteHelper.class);

  private ExcelWriteHelper() {
    // default constructor
  }

  /**
   * write workbook to supplied output stream
   *
   * @param outputStream  notice close the stream
   * @param excelWorkbook intend to write workbook
   * @throws IOException io exception
   */
  public static void write(OutputStream outputStream, Workbook excelWorkbook) throws IOException {
    if (excelWorkbook == null) {
      LOGGER.error("workbook is null");
      throw new ExcelWriteException("workbook is null");
    }

    org.apache.poi.ss.usermodel.Workbook workbook = createWorkbook(excelWorkbook);

    for (Sheet excelSheet : excelWorkbook.getSheets()) {

      org.apache.poi.ss.usermodel.Sheet sheet = createSheet(workbook, excelSheet);

      for (Row excelRow : excelSheet.getRows()) {

        org.apache.poi.ss.usermodel.Row row = createRow(sheet);
        createRowCells(row, excelRow);
      }
    }

    workbook.write(outputStream);
    if (workbook instanceof SXSSFWorkbook) {
      ((SXSSFWorkbook) workbook).dispose();
    }
  }

  private static org.apache.poi.ss.usermodel.Workbook createWorkbook(Workbook workbook) {
    if (workbook.isAfter97()) {
      return new HSSFWorkbook();
    } else {
      // keep 100 rows in memory, exceeding rows will be flushed to disk
      return new SXSSFWorkbook(100);
    }
  }

  private static org.apache.poi.ss.usermodel.Sheet createSheet(org.apache.poi.ss.usermodel.Workbook workbook, Sheet sheet) {
    String sheetName = sheet.getName();

    if (StringUtils.isBlank(sheetName)) {
      workbook.createSheet();
    } else {
      workbook.createSheet(sheetName);
    }

    return workbook.getSheetAt(workbook.getNumberOfSheets() - 1);
  }

  private static org.apache.poi.ss.usermodel.Row createRow(org.apache.poi.ss.usermodel.Sheet sheet) {
    org.apache.poi.ss.usermodel.Row row;
    if (sheet.getPhysicalNumberOfRows() == 0) {
      row = sheet.createRow(0);
    } else {
      row = sheet.createRow(sheet.getLastRowNum() + 1);
    }
    return row;
  }

  private static void createRowCells(org.apache.poi.ss.usermodel.Row row, Row excelRow) {
    for (int i = 0; i < excelRow.sizeOfCells(); i++) {

      Cell excelCell = excelRow.getCell(i + 1);
      String value = excelCell.getValue();

      org.apache.poi.ss.usermodel.Cell cell = row.createCell(i, org.apache.poi.ss.usermodel.Cell.CELL_TYPE_STRING);
      cell.setCellValue(value == null ? ExcelConstants.EMPTY_VALUE : value);
    }
  }

}
