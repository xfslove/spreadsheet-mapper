package excel.engine.f2w;

import excel.engine.model.core.*;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;

public class Excel2WorkbookReader implements WorkbookReader {

  private static final Logger LOGGER = LoggerFactory.getLogger(Excel2WorkbookReader.class);

  public Workbook read(InputStream inputStream) throws IOException {

    Workbook excelWorkbook = new WorkbookBean();

    try {

      if (inputStream.available() == 0) {
        return excelWorkbook;
      }

      org.apache.poi.ss.usermodel.Workbook workbook = WorkbookFactory.create(inputStream);

      int sheetCount = workbook.getNumberOfSheets();

      for (int i = 0; i < sheetCount; i++) {

        org.apache.poi.ss.usermodel.Sheet sheet = workbook.getSheetAt(i);

        if (sheet == null) {
          continue;
        }

        Sheet excelSheet = createSheet(sheet);
        excelWorkbook.addSheet(excelSheet);

        int lastRowNum = sheet.getLastRowNum();
        for (int j = 0; j < lastRowNum; j++) {

          org.apache.poi.ss.usermodel.Row row = sheet.getRow(j);

          Row excelRow = createRow(row);
          excelSheet.addRow(excelRow);

          createRowCells(row, excelRow);
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

  private static Sheet createSheet(org.apache.poi.ss.usermodel.Sheet sheet) {
    return new SheetBean(sheet);
  }

  private static Row createRow(org.apache.poi.ss.usermodel.Row row) {
    return new RowBean(row);
  }

  private static void createRowCells(org.apache.poi.ss.usermodel.Row row, Row excelRow) {
    int maxColNum = row.getLastCellNum() > 0 ? row.getLastCellNum() : 0;

    for (int k = 0; k < maxColNum; k++) {

      org.apache.poi.ss.usermodel.Cell cell = row.getCell(k);

      CellBean excelCell;
      if (cell == null) {

        excelCell = CellBean.EMPTY_CELL(k + 1, row.getRowNum() + 1);
      } else {

        excelCell = new CellBean(cell);
      }

      excelRow.addCell(excelCell);
    }
  }
}
