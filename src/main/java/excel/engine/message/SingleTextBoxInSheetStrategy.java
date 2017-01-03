package excel.engine.message;

import excel.engine.Constants;
import excel.engine.model.message.ErrorMessage;
import excel.engine.model.message.MessageWriteStrategies;
import excel.engine.model.shapes.TextBox;
import excel.engine.model.shapes.TextBoxBean;
import excel.engine.model.shapes.TextBoxStyle;
import excel.engine.w2f.WorkbookWriteException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.OutputStream;
import java.util.*;

/**
 * use text box to write error message strategy and one sheet one text box
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class SingleTextBoxInSheetStrategy implements MessageWriteStrategy {

  private static final Logger LOGGER = LoggerFactory.getLogger(SingleTextBoxInSheetStrategy.class);

  private Workbook workbook;

  public SingleTextBoxInSheetStrategy(Workbook workbook) {
    this.workbook = workbook;
  }

  @Override
  public String getStrategy() {
    return MessageWriteStrategies.TEXT_BOX;
  }

  @Override
  public void write(OutputStream outputStream, Collection<ErrorMessage> errorMessages) {

    List<TextBox> textBoxes = transferToTextBoxes(errorMessages);

    for (TextBox textBox : textBoxes) {

      int numberOfSheets = workbook.getNumberOfSheets();

      while (numberOfSheets < textBox.getSheetIndex()) {
        workbook.createSheet();
        numberOfSheets = workbook.getNumberOfSheets();
      }

      Sheet sheet = workbook.getSheetAt(textBox.getSheetIndex() - 1);
      if (sheet instanceof XSSFSheet) {
        addXSSFTextBox(sheet, textBox);
      } else {
        addHSSFTextBox(sheet, textBox);
      }
    }

    try {
      workbook.write(outputStream);
    } catch (IOException e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookWriteException(e);
    }

  }

  private List<TextBox> transferToTextBoxes(Collection<ErrorMessage> errorMessages) {

    Map<Integer, List<String>> textBoxMessageMap = new HashMap<>();
    for (ErrorMessage errorMessage : errorMessages) {

      int sheetIndex = errorMessage.getSheetIndex();

      if (!textBoxMessageMap.containsKey(sheetIndex)) {
        textBoxMessageMap.put(sheetIndex, new ArrayList<String>());
      }
      textBoxMessageMap.get(sheetIndex).add(errorMessage.getErrorMessage());
    }

    List<TextBox> textBoxes = new ArrayList<>();
    for (Map.Entry<Integer, List<String>> entry : textBoxMessageMap.entrySet()) {
      textBoxes.add(new TextBoxBean(StringUtils.join(entry.getValue(), Constants.COMMA_SEPARATOR), entry.getKey()));
    }

    return textBoxes;
  }

  private void addXSSFTextBox(Sheet sheet, TextBox textBox) {
    TextBoxStyle style = textBox.getStyle();

    XSSFSheet xssfSheet = (XSSFSheet) sheet;
    XSSFDrawing drawingPatriarch = xssfSheet.createDrawingPatriarch();
    XSSFClientAnchor anchor = drawingPatriarch.createAnchor(0, 0, 0, 0, style.getCol1() - 1, style.getRow1() - 1, style.getCol2() - 1, style.getRow2() - 1);

    XSSFTextBox textbox = drawingPatriarch.createTextbox(anchor);
    textbox.setText(new XSSFRichTextString(textBox.getMessage()));
    textbox.setFillColor(style.getRed(), style.getGreen(), style.getBlue());

  }

  private void addHSSFTextBox(Sheet sheet, TextBox textBox) {
    TextBoxStyle style = textBox.getStyle();

    HSSFSheet hssfSheet = (HSSFSheet) sheet;
    HSSFPatriarch drawingPatriarch = hssfSheet.createDrawingPatriarch();
    HSSFClientAnchor anchor = drawingPatriarch.createAnchor(0, 0, 0, 0, style.getCol1() - 1, style.getRow1() - 1, style.getCol2() - 1, style.getRow2() - 1);

    HSSFTextbox textbox = drawingPatriarch.createTextbox(anchor);
    textbox.setString(new HSSFRichTextString(textBox.getMessage()));
    textbox.setFillColor(style.getRed(), style.getGreen(), style.getBlue());

  }
}
