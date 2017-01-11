package spreadsheet.mapper.m2f;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.*;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.*;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.model.shapes.TextBox;
import spreadsheet.mapper.model.shapes.TextBoxBean;
import spreadsheet.mapper.model.shapes.TextBoxStyle;

import java.util.*;

/**
 * use text box to write error message strategy and one sheet one text box
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class SingleTextBoxInSheetStrategy implements MessageWriteStrategy {

  @Override
  public String getStrategy() {
    return MessageWriteStrategies.TEXT_BOX;
  }

  @Override
  public void write(Workbook workbook, Collection<ErrorMessage> errorMessages) {

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
      textBoxes.add(new TextBoxBean(StringUtils.join(entry.getValue(), Constants.ENTER_SEPARATOR), entry.getKey()));
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
