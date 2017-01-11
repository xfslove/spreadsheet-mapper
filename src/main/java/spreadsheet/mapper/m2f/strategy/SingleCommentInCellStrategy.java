package spreadsheet.mapper.m2f.strategy;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.*;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.model.shapes.CommentBean;

import java.util.*;

/**
 * use comment to write error message strategy and one cell one comment
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class SingleCommentInCellStrategy implements MessageWriteStrategy {

  @Override
  public String getStrategy() {
    return MessageWriteStrategies.COMMENT;
  }

  @Override
  public void write(Workbook workbook, Collection<ErrorMessage> errorMessages) {

    if (CollectionUtils.isEmpty(errorMessages)) {
      return;
    }

    List<spreadsheet.mapper.model.shapes.Comment> comments = transferToComments(errorMessages);

    for (spreadsheet.mapper.model.shapes.Comment comment : comments) {

      int numberOfSheets = workbook.getNumberOfSheets();

      while (numberOfSheets < comment.getSheetIndex()) {
        workbook.createSheet();
        numberOfSheets = workbook.getNumberOfSheets();
      }

      Sheet sheet = workbook.getSheetAt(comment.getSheetIndex() - 1);
      addComment(sheet, comment);
    }
  }

  private List<spreadsheet.mapper.model.shapes.Comment> transferToComments(Collection<ErrorMessage> errorMessages) {

    // sheet -> row -> column -> messages
    Map<Integer, Map<Integer, Map<Integer, List<String>>>> commentMessageMap = new HashMap<>();

    for (ErrorMessage errorMessage : errorMessages) {

      int sheetIndex = errorMessage.getSheetIndex();
      int rowIndex = errorMessage.getRowIndex();
      int columnIndex = errorMessage.getColumnIndex();

      if (!commentMessageMap.containsKey(sheetIndex)) {
        commentMessageMap.put(sheetIndex, new HashMap<Integer, Map<Integer, List<String>>>());
      }
      Map<Integer, Map<Integer, List<String>>> commentRowMap = commentMessageMap.get(sheetIndex);

      if (!commentRowMap.containsKey(rowIndex)) {
        commentRowMap.put(rowIndex, new HashMap<Integer, List<String>>());
      }
      Map<Integer, List<String>> commentColumnMap = commentRowMap.get(rowIndex);

      if (!commentColumnMap.containsKey(columnIndex)) {
        commentColumnMap.put(columnIndex, new ArrayList<String>());
      }
      commentColumnMap.get(columnIndex).add(errorMessage.getErrorMessage());
    }

    List<spreadsheet.mapper.model.shapes.Comment> comments = new ArrayList<>();

    for (Map.Entry<Integer, Map<Integer, Map<Integer, List<String>>>> sheetEntry : commentMessageMap.entrySet()) {

      for (Map.Entry<Integer, Map<Integer, List<String>>> rowEntry : sheetEntry.getValue().entrySet()) {

        for (Map.Entry<Integer, List<String>> columnEntry : rowEntry.getValue().entrySet()) {

          comments.add(
              new CommentBean(StringUtils.join(columnEntry.getValue(), Constants.ENTER_SEPARATOR), sheetEntry.getKey(), rowEntry.getKey(), columnEntry.getKey()));

        }
      }
    }

    return comments;
  }

  private void addComment(Sheet sheet, spreadsheet.mapper.model.shapes.Comment comment) {
    int rowIndex = comment.getRowIndex();
    int colIndex = comment.getColumnIndex();

    Row row = sheet.getRow(rowIndex - 1);
    if (row == null) {
      row = sheet.createRow(rowIndex - 1);
    }
    Cell cell = row.getCell(colIndex - 1);
    if (cell == null) {
      cell = row.createCell(colIndex - 1, Cell.CELL_TYPE_STRING);
    }

    CreationHelper factory = sheet.getWorkbook().getCreationHelper();
    ClientAnchor anchor = factory.createClientAnchor();
    // When the comment box is visible
    anchor.setCol1(colIndex - 1);
    anchor.setRow1(rowIndex - 1);
    anchor.setCol2(colIndex - 1 + comment.getHeight());
    anchor.setRow2(rowIndex - 1 + comment.getLength());

    // Create the comment and set the text
    Drawing drawing = sheet.createDrawingPatriarch();
    org.apache.poi.ss.usermodel.Comment poiComment = drawing.createCellComment(anchor);
    RichTextString str = factory.createRichTextString(comment.getMessage());
    poiComment.setString(str);

    // Assign the new comment to the cell
    cell.setCellComment(poiComment);

  }
}
