package me.excel.tools.helper;

import me.excel.tools.ExcelConstants;
import me.excel.tools.exception.ExcelReadException;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
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
public class WorkbookToExcelHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(WorkbookToExcelHelper.class);

  private WorkbookToExcelHelper() {
    // default constructor
  }

  /**
   * write workbook to supplied output stream
   *
   * @param excelWorkbook intend to write workbook
   * @param outputStream  notice close the stream
   * @throws IOException io exception
   */
  public static void write(ExcelWorkbook excelWorkbook, OutputStream outputStream) throws IOException {
    if (excelWorkbook == null) {
      LOGGER.error("workbook is null");
      throw new ExcelReadException("workbook is null");
    }

    Workbook workbook = createWorkbook(excelWorkbook);

    for (ExcelSheet excelSheet : excelWorkbook.getSheets()) {

      Sheet sheet = createSheet(workbook, excelSheet);

      for (ExcelRow excelRow : excelSheet.getRows()) {
        createRowAndCells(sheet, excelRow);
      }
    }

    workbook.write(outputStream);
    if (workbook instanceof SXSSFWorkbook) {
      ((SXSSFWorkbook) workbook).dispose();
    }
  }

  private static Workbook createWorkbook(ExcelWorkbook excelWorkbook) {
    if (excelWorkbook.isAfter97()) {
      return new HSSFWorkbook();
    } else {
      // keep 100 rows in memory, exceeding rows will be flushed to disk
      return new SXSSFWorkbook(100);
    }
  }

  private static Sheet createSheet(Workbook workbook, ExcelSheet excelSheet) {
    String sheetName = excelSheet.getSheetName();

    if (StringUtils.isBlank(sheetName)) {
      workbook.createSheet();
    } else {
      workbook.createSheet(sheetName);
    }

    return workbook.getSheetAt(workbook.getNumberOfSheets() - 1);
  }

  private static void createRowAndCells(Sheet sheet, ExcelRow excelRow) {

    Row row;
    if (sheet.getPhysicalNumberOfRows() == 0) {
      row = sheet.createRow(0);
    } else {
      row = sheet.createRow(sheet.getLastRowNum() + 1);
    }

    for (int i = 0; i < excelRow.sizeOfCells(); i++) {

      ExcelCell excelCell = excelRow.getCell(i + 1);
      String value = excelCell.getValue();

      Cell cell = row.createCell(i, Cell.CELL_TYPE_STRING);
      cell.setCellValue(value == null ? ExcelConstants.EMPTY_VALUE : value);
    }
  }

}
