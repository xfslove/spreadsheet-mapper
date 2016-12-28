package me.excel.tools.helper;

import me.excel.tools.exception.ExcelWriteException;
import me.excel.tools.model.extra.TextBox;
import me.excel.tools.model.extra.TextBoxStyle;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.Collection;

/**
 * write text box to excel util
 * <p>
 * Created by hanwen on 2016/12/27.
 */
public class ExcelTextBoxHelper {

  private static final Logger LOGGER = LoggerFactory.getLogger(ExcelTextBoxHelper.class);

  private ExcelTextBoxHelper() {
    // default constructor
  }

  /**
   * write text box
   *
   * @param file     intend write file
   * @param textBoxs text boxs
   * @see #writeTextBox(InputStream, OutputStream, Collection)
   */
  public static void writeTextBox(File file, Collection<TextBox> textBoxs) {

    try (Workbook workbook = WorkbookFactory.create(new FileInputStream(file))) {

      addTextBoxs(workbook, textBoxs);
      try (FileOutputStream outputStream = new FileOutputStream(file)) {
        workbook.write(outputStream);
      }

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelWriteException(e);
    }
  }

  /**
   * write text boxs
   *
   * @param inputStream  auto close
   * @param outputStream intend write stream, notice close
   * @param textBoxs     text boxs
   */
  public static void writeTextBox(InputStream inputStream, OutputStream outputStream, Collection<TextBox> textBoxs) {

    try (Workbook workbook = WorkbookFactory.create(inputStream)) {

      addTextBoxs(workbook, textBoxs);
      workbook.write(outputStream);

    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelWriteException(e);
    }
  }

  private static void addTextBoxs(Workbook workbook, Collection<TextBox> textBoxs) {

    int numberOfSheets = workbook.getNumberOfSheets();

    for (TextBox textBox : textBoxs) {

      if (numberOfSheets < textBox.getSheetIndex()) {
        throw new IllegalArgumentException("index of sheet text box at are out of bounds");
      }

      Sheet sheet = workbook.getSheetAt(textBox.getSheetIndex() - 1);

      if (sheet instanceof XSSFSheet) {
        addXSSFTextBox(sheet, textBox);
      } else {
        addHSSFTextBox(sheet, textBox);
      }

    }

  }

  private static void addXSSFTextBox(Sheet sheet, TextBox textBox) {
    TextBoxStyle style = textBox.getStyle();

    XSSFSheet xssfSheet = (XSSFSheet) sheet;
    XSSFDrawing drawingPatriarch = xssfSheet.createDrawingPatriarch();
    XSSFClientAnchor anchor = drawingPatriarch.createAnchor(0, 0, 0, 0, style.getCol1(), style.getRow1(), style.getCol2(), style.getRow2());

    XSSFTextBox textbox = drawingPatriarch.createTextbox(anchor);
    textbox.setText(new XSSFRichTextString(textBox.getMessage()));
    textbox.setFillColor(style.getRed(), style.getGreen(), style.getBlue());

  }

  private static void addHSSFTextBox(Sheet sheet, TextBox textBox) {
    TextBoxStyle style = textBox.getStyle();

    HSSFSheet hssfSheet = (HSSFSheet) sheet;
    HSSFPatriarch drawingPatriarch = hssfSheet.createDrawingPatriarch();
    HSSFClientAnchor anchor = drawingPatriarch.createAnchor(0, 0, 0, 0, style.getCol1(), style.getRow1(), style.getCol2(), style.getRow2());

    HSSFTextbox textbox = drawingPatriarch.createTextbox(anchor);
    textbox.setString(new HSSFRichTextString(textBox.getMessage()));
    textbox.setFillColor(style.getRed(), style.getGreen(), style.getBlue());

  }
}
