package excel.engine.w2f;

import excel.engine.ExcelConstants;
import excel.engine.model.core.Cell;
import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;
import excel.engine.model.core.Workbook;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.OutputStream;

/**
 * workbook to excel writer adapter
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public abstract class Workbook2ExcelWriterAdapter implements WorkbookWriter {

  public void write(Workbook workbook, OutputStream outputStream) throws IOException {
    if (workbook == null) {
      return;
    }

    beforeWrite();

    org.apache.poi.ss.usermodel.Workbook poiWorkbook = createWorkbook(workbook);

    for (Sheet excelSheet : workbook.getSheets()) {

      org.apache.poi.ss.usermodel.Sheet sheet = createSheet(poiWorkbook, excelSheet);

      for (Row excelRow : excelSheet.getRows()) {

        org.apache.poi.ss.usermodel.Row row = createRow(sheet);
        createRowCells(row, excelRow);
      }
    }

    poiWorkbook.write(outputStream);

    afterWrite(poiWorkbook);
  }

  protected abstract org.apache.poi.ss.usermodel.Workbook createWorkbook(Workbook workbook);

  protected void afterWrite(org.apache.poi.ss.usermodel.Workbook workbook) {
    // no thing
  }

  protected void beforeWrite() {
    // no thing
  }

  private org.apache.poi.ss.usermodel.Sheet createSheet(org.apache.poi.ss.usermodel.Workbook workbook, Sheet sheet) {
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

  private void createRowCells(org.apache.poi.ss.usermodel.Row row, Row excelRow) {
    for (int i = 0; i < excelRow.sizeOfCells(); i++) {

      Cell excelCell = excelRow.getCell(i + 1);
      String value = excelCell.getValue();

      org.apache.poi.ss.usermodel.Cell cell = row.createCell(i, org.apache.poi.ss.usermodel.Cell.CELL_TYPE_STRING);
      cell.setCellValue(value == null ? ExcelConstants.EMPTY_VALUE : value);
    }
  }

}
