package spread.sheet.w2f;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.Constants;
import spread.sheet.model.core.Cell;
import spread.sheet.model.core.Row;
import spread.sheet.model.core.Sheet;
import spread.sheet.model.core.Workbook;

import java.io.IOException;
import java.io.OutputStream;

/**
 * workbook to excel writer adapter
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public abstract class Workbook2ExcelWriterAdapter implements WorkbookWriter {

  private static final Logger LOGGER = LoggerFactory.getLogger(Workbook2ExcelWriterAdapter.class);

  public void write(Workbook workbook, OutputStream outputStream) {
    if (workbook == null) {
      return;
    }

    beforeWrite();

    org.apache.poi.ss.usermodel.Workbook poiWorkbook = createWorkbook(workbook);

    for (Sheet excelSheet : workbook.getSheets()) {

      org.apache.poi.ss.usermodel.Sheet sheet = createSheet(poiWorkbook, excelSheet);

      for (Row excelRow : excelSheet.getRows()) {

        org.apache.poi.ss.usermodel.Row row = createRow(sheet, excelRow);

        for (Cell excelCell : excelRow.getCells()) {
          createCell(row, excelCell);
        }
      }
    }

    try {
      poiWorkbook.write(outputStream);
    } catch (IOException e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookWriteException(e);
    }

    afterWrite(poiWorkbook);
  }

  protected void afterWrite(org.apache.poi.ss.usermodel.Workbook workbook) {
    // nothing
  }

  protected void beforeWrite() {
    // nothing
  }

  protected abstract org.apache.poi.ss.usermodel.Workbook createWorkbook(Workbook workbook);

  private org.apache.poi.ss.usermodel.Sheet createSheet(org.apache.poi.ss.usermodel.Workbook workbook, Sheet sheet) {
    String sheetName = sheet.getName();

    org.apache.poi.ss.usermodel.Sheet poiSheet;
    if (StringUtils.isBlank(sheetName)) {
      poiSheet = workbook.createSheet();
    } else {
      poiSheet = workbook.createSheet(sheetName);
    }

    return poiSheet;
  }

  private org.apache.poi.ss.usermodel.Row createRow(org.apache.poi.ss.usermodel.Sheet sheet, Row excelRow) {
    return sheet.createRow(excelRow.getIndex() - 1);
  }

  private void createCell(org.apache.poi.ss.usermodel.Row row, Cell excelCell) {
    String value = excelCell.getValue();
    org.apache.poi.ss.usermodel.Cell cell = row.createCell(excelCell.getIndex() - 1, org.apache.poi.ss.usermodel.Cell.CELL_TYPE_STRING);
    cell.setCellValue(value == null ? Constants.EMPTY_VALUE : value);
  }

}
