package me.excel.tools.exporter;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import me.excel.tools.model.excel.ExcelWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Created by hanwen on 15-12-16.
 */
public class ExcelFileExporter implements UserFileExporter {

  private ExcelWorkbook excelWorkbook;

  private Workbook workbook;

  public ExcelFileExporter(ExcelWorkbook excelWorkbook) {
    this.excelWorkbook = excelWorkbook;
  }

  @Override
  public void export(OutputStream outputStream) throws IOException {
    if (this.excelWorkbook == null) {
      throw new IllegalArgumentException("workbook is null");
    }

    createWorkbook();

    excelWorkbook.getSheets().forEach(excelSheet -> {
      createSheet(excelSheet);
      int numberOfSheets = this.workbook.getNumberOfSheets();
      Sheet lastSheet = this.workbook.getSheetAt(numberOfSheets - 1);
      excelSheet.getRows().forEach(row -> createRowAndCells(lastSheet, row));
    });

    workbook.write(outputStream);
    if (this.workbook instanceof SXSSFWorkbook) {
      ((SXSSFWorkbook) workbook).dispose();
    }
  }

  private void createWorkbook() {
    boolean hasComments = false;
    for (ExcelSheet excelSheet : excelWorkbook.getSheets()) {
      if (excelSheet.hasComments()) {
        hasComments = true;
        break;
      }
    }

    if (hasComments) {
      this.workbook = new XSSFWorkbook();
    } else {
      // keep 100 rows in memory, exceeding rows will be flushed to disk
      this.workbook = new SXSSFWorkbook(100);
    }
  }

  private void createSheet(ExcelSheet excelSheet) {
    String sheetName = excelSheet.getSheetName();
    if (sheetName == null) {
      this.workbook.createSheet();
    } else {
      this.workbook.createSheet(sheetName);
    }
  }

  private void createRowAndCells(Sheet sheet, ExcelRow excelRow) {
    Row row;
    int numberOfSheets = this.workbook.getNumberOfSheets();
    Sheet currentSheet = this.workbook.getSheetAt(numberOfSheets - 1);
    if (currentSheet.getPhysicalNumberOfRows() == 0) {
      row = currentSheet.createRow(0);
    } else {
      row = currentSheet.createRow(currentSheet.getLastRowNum() + 1);
    }

    for (int i = 0; i < excelRow.sizeOfCells(); i++) {
      ExcelCell excelCell = excelRow.getCell(i + 1);
      String value = excelCell.getValue();
      Cell cell = row.createCell(i, Cell.CELL_TYPE_STRING);
      cell.setCellValue(value == null ? ExcelConstants.EMPTY_VALUE : value);

      // add comments
      ExcelCommentUtils.addComment(sheet, excelCell.getComment());
    }
  }

}
