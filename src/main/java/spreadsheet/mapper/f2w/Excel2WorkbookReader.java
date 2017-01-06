package spreadsheet.mapper.f2w;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.NumberToTextConverter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.model.core.*;
import spreadsheet.mapper.utils.DateFormatRegister;

import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;

/**
 * excel to workbook reader decorator
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class Excel2WorkbookReader implements WorkbookReader {

  private static final Logger LOGGER = LoggerFactory.getLogger(Excel2WorkbookReader.class);

  private org.apache.poi.ss.usermodel.Workbook workbook;

  public Workbook read(InputStream inputStream) {

    Workbook excelWorkbook = new WorkbookBean();
    try {

      if (inputStream.available() == 0) {
        return excelWorkbook;
      }

      workbook = WorkbookFactory.create(inputStream);

      int sheetCount = workbook.getNumberOfSheets();

      for (int i = 0; i < sheetCount; i++) {

        org.apache.poi.ss.usermodel.Sheet sheet = workbook.getSheetAt(i);

        if (sheet == null) {
          continue;
        }

        Sheet excelSheet = createSheet(sheet);
        excelWorkbook.addSheet(excelSheet);

        int lastRowNum = sheet.getLastRowNum();
        for (int j = 0; j <= lastRowNum; j++) {

          org.apache.poi.ss.usermodel.Row row = sheet.getRow(j);
          Row excelRow = createRow();
          excelSheet.addRow(excelRow);

          int lastColumnNum = row.getLastCellNum() > 0 ? row.getLastCellNum() : 0;
          for (int k = 0; k < lastColumnNum; k++) {

            org.apache.poi.ss.usermodel.Cell cell = row.getCell(k);
            excelRow.addCell(createCell(cell));
          }
        }
      }

      return excelWorkbook;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookReadException(e);
    } finally {

      try {
        workbook.close();
      } catch (IOException e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
      }
    }
  }

  private Sheet createSheet(org.apache.poi.ss.usermodel.Sheet sheet) {
    String sheetName = sheet.getSheetName();

    if (StringUtils.isBlank(sheetName)) {

      return new SheetBean();
    }

    return new SheetBean(sheetName);
  }

  private Row createRow() {
    return new RowBean();
  }

  private Cell createCell(org.apache.poi.ss.usermodel.Cell cell) {
    if (cell == null) {

      return new CellBean();
    }

    String value;

    if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_BLANK) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_BOOLEAN) {

      value = Boolean.toString(cell.getBooleanCellValue());

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_ERROR) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_FORMULA) {

      value = null;

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_NUMERIC) {

      if (DateUtil.isCellDateFormatted(cell)) {
        String dateFormat = DateFormatRegister.get(cell.getCellStyle().getDataFormatString());

        if (dateFormat == null) {
          value = DateFormatRegister.ERROR_PATTERN;
        } else {
          SimpleDateFormat simpleDateFormat = new SimpleDateFormat(dateFormat);
          value = simpleDateFormat.format(cell.getDateCellValue());
        }
      } else {
        value = NumberToTextConverter.toText(cell.getNumericCellValue());
      }

    } else if (cell.getCellType() == org.apache.poi.ss.usermodel.Cell.CELL_TYPE_STRING) {

      String cellValue = cell.getStringCellValue();
      value = StringUtils.isBlank(cellValue) ? null : cellValue.trim();

    } else {

      value = null;

    }

    return new CellBean(value);

  }

}
