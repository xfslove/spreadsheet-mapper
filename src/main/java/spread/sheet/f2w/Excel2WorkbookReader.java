package spread.sheet.f2w;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spread.sheet.model.core.*;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by hanwen on 2017/1/3.
 */
public class Excel2WorkbookReader implements WorkbookReader {

  private static final Logger LOGGER = LoggerFactory.getLogger(Excel2WorkbookReader.class);

  public Workbook read(InputStream inputStream) {

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

          int lastColumnNum = row.getLastCellNum() > 0 ? row.getLastCellNum() : 0;
          for (int k = 0; k < lastColumnNum; k++) {

            createCell(row, excelRow, k);
          }
        }
      }

      return excelWorkbook;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookReadException(e);
    } finally {

      try {
        inputStream.close();
      } catch (IOException e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
      }
    }
  }

  private Sheet createSheet(org.apache.poi.ss.usermodel.Sheet sheet) {
    return new SheetBean(sheet);
  }

  private Row createRow(org.apache.poi.ss.usermodel.Row row) {
    return new RowBean(row);
  }

  private void createCell(org.apache.poi.ss.usermodel.Row row, Row excelRow, int columnIndex) {
    org.apache.poi.ss.usermodel.Cell cell = row.getCell(columnIndex);

    CellBean excelCell;
    if (cell == null) {

      excelCell = CellBean.EMPTY_CELL(columnIndex + 1);
    } else {

      excelCell = new CellBean(cell);
    }

    excelRow.addCell(excelCell);
  }
}
